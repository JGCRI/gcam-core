#include <sys/resource.h>

#include <arrow/buffer.h>

#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string_view>
#include <vector>

constexpr std::size_t periods = 8;
constexpr int passes = 20;

struct Parameters {
    std::uint64_t name;
    std::uint64_t unit;
    double coefficient;
    double input_emissions;
};

Parameters parameters_for(std::size_t index) {
    return {index % 31, index % 3, 0.001 * (index + 1), 0.1 * (index % 17)};
}

// Models the allocation pattern, rather than the ABI, of GCAM's emissions objects.
struct Emissions {
    virtual ~Emissions() = default;
    Parameters parameters;
    std::unique_ptr<double[]> values = std::make_unique<double[]>(periods);
    std::uint32_t first_period = 0;
    std::uint32_t period_count = periods;
};

template<class T>
struct Column {
    std::size_t size;
    std::unique_ptr<arrow::Buffer> buffer;
    T* data;

    explicit Column(std::size_t rows) : size(rows) {
        if (rows == 0) {
            data = nullptr;
            return;
        }
        auto result = arrow::AllocateBuffer(rows * sizeof(T));
        if (!result.ok()) {
            throw std::runtime_error(result.status().ToString());
        }
        buffer = std::move(result).ValueOrDie();
        data = reinterpret_cast<T*>(buffer->mutable_data());
        std::uninitialized_value_construct_n(data, rows);
    }

    T& operator[](std::size_t row) { return data[row]; }
};

struct ParameterColumns {
    Column<std::uint64_t> names, units;
    Column<double> coefficients, inputs;

    explicit ParameterColumns(std::size_t size)
        : names(size), units(size), coefficients(size), inputs(size) {}

    void set(std::size_t row, const Parameters& parameters) {
        names[row] = parameters.name;
        units[row] = parameters.unit;
        coefficients[row] = parameters.coefficient;
        inputs[row] = parameters.input_emissions;
    }
};

double peak_rss_mib() {
    rusage usage{};
    if (getrusage(RUSAGE_SELF, &usage) != 0) {
        std::abort();
    }
#ifdef __APPLE__
    return usage.ru_maxrss / (1024.0 * 1024.0);
#else
    return usage.ru_maxrss / 1024.0;
#endif
}

int main(int argc, char** argv) {
    if (argc != 4) {
        std::cerr << "Usage: emissions_layout {objects|columns|shared} ROWS DISTINCT_ROWS\n";
        return 2;
    }
    const std::string_view mode = argv[1];
    if (mode != "objects" && mode != "columns" && mode != "shared") return 2;
    const auto count = std::strtoull(argv[2], nullptr, 10);
    const auto distinct = std::strtoull(argv[3], nullptr, 10);
    if (distinct == 0 || distinct > count || count > UINT32_MAX) return 2;

    std::vector<std::unique_ptr<Emissions>> objects;
    const bool shared = mode == "shared";
    ParameterColumns columns(mode == "objects" ? 0 : shared ? distinct : count);
    Column<std::uint32_t> row_ids(shared ? count : 0);
    Column<double> values(mode == "objects" ? 0 : count * periods);

    if (mode == "objects") {
        objects.reserve(count);
        for (std::size_t i = 0; i < count; ++i) {
            auto object = std::make_unique<Emissions>();
            object->parameters = parameters_for(i % distinct);
            objects.push_back(std::move(object));
        }
    } else {
        for (std::size_t i = 0; i < columns.coefficients.size; ++i) {
            columns.set(i, parameters_for(i % distinct));
        }
        for (std::size_t i = 0; i < row_ids.size; ++i) {
            row_ids[i] = static_cast<std::uint32_t>(i % distinct);
        }
    }

    const auto start = std::chrono::steady_clock::now();
    for (int pass = 0; pass < passes; ++pass) {
        const auto period = static_cast<std::size_t>(pass) % periods;
        const auto activity = 1.0 + pass * 0.01;
        if (mode == "objects") {
            for (const auto& object : objects) {
                object->values[period] =
                    object->parameters.coefficient * activity + object->parameters.input_emissions;
            }
        } else if (shared) {
            for (std::size_t i = 0; i < count; ++i) {
                const auto row = row_ids[i];
                values[period * count + i] = columns.coefficients[row] * activity + columns.inputs[row];
            }
        } else {
            for (std::size_t i = 0; i < count; ++i) {
                values[period * count + i] = columns.coefficients[i] * activity + columns.inputs[i];
            }
        }
    }
    const auto end = std::chrono::steady_clock::now();
    const auto peak = peak_rss_mib();
    double checksum = 0;
    for (std::size_t period = 0; period < periods; ++period) {
        for (std::size_t i = 0; i < count; ++i) {
            checksum += mode == "objects" ? objects[i]->values[period] : values[period * count + i];
        }
    }
    std::cout << mode << ',' << count << ',' << distinct << ',' << std::fixed << std::setprecision(2)
              << peak << ','
              << std::chrono::duration<double, std::milli>(end - start).count() << ','
              << std::setprecision(9) << checksum << '\n';
}
