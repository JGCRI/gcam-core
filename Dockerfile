# syntax=docker/dockerfile:1-labs
FROM debian:bookworm-slim

LABEL org.opencontainers.image.authors="Matt Jensen <docker@publicmatt.com>"
LABEL version="1.0"
LABEL description="Builds GCAM core for debian from scratch."

########################################
#
# install dependencies
#
########################################
RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake \
    curl \
    default-jre \ 
    default-jdk \
    g++ \
    gcc \
    git \
    libtbb-dev \
    make \
    unzip \
    wget && \
    apt clean && \
    rm -rf /var/lib/apt && \
    rm -rf /var/lib/dpkg/info/*

########################################
#
# download gcam core
#
########################################
ENV CORE=/gcam-core
RUN git clone https://github.com/JGCRI/gcam-core.git --depth=1 --branch gcam-v7.0 ${CORE}

########################################
#
# install hector
#
########################################
WORKDIR ${CORE}
RUN make install_hector

########################################
#
# download and extract libs
#
########################################
ENV LIBS=${CORE}/libs
RUN mkdir -p ${LIBS}
WORKDIR /tmp
RUN wget https://github.com/JGCRI/modelinterface/releases/download/v5.4/jars.zip -O jars.zip
RUN wget https://github.com/oneapi-src/oneTBB/archive/refs/tags/v2021.9.0.zip -O tbb.zip
RUN wget https://gitlab.com/libeigen/eigen/-/archive/3.4.0/eigen-3.4.0.zip -O eigen.zip
RUN wget https://boostorg.jfrog.io/artifactory/main/release/1.82.0/source/boost_1_82_0.zip -O boost.zip

RUN unzip tbb.zip && mv oneTBB-2021.9.0 ${LIBS}/tbb
RUN unzip eigen.zip && mv eigen-3.4.0 ${LIBS}/eigen
RUN unzip jars.zip && mv jars ${LIBS}/jars
RUN unzip boost.zip && mv boost_1_82_0 ${LIBS}/boost-lib


########################################
#
# download and install boost
#
########################################
# ADD https://boostorg.jfrog.io/artifactory/main/release/1.82.0/source/boost_1_82_0.zip ${LIBS}/boost-lib
WORKDIR ${LIBS}/boost-lib
RUN ./bootstrap.sh --with-libraries=system,filesystem --prefix=${LIBS}/boost-lib/stage/lib
RUN ./b2 stage

########################################
#
# build gcam-core
#
########################################
ENV JAVA_DIR=/usr/lib/jvm/default-java
ENV CXX=g++
# for boost
ENV BOOST_INCLUDE=${LIBS}/boost-lib
ENV BOOST_LIB=${LIBS}/boost-lib/stage/lib
# For Hector, which uses different definitions
ENV BOOSTLIB=${LIBS}/boost-lib
ENV BOOSTROOT=${BOOST_INCLUDE}
ENV JAVA_INCLUDE=${JAVA_DIR}/include
ENV JAVA_LIB=${JAVA_DIR}/lib/server
ENV JARS_LIB=${LIBS}/jars/*
ENV EIGEN_INCLUDE=${LIBS}/eigen
ENV TBB_INCLUDE=${LIBS}/tbb/include/tbb
ENV TBB_LIB=${LIBS}/tbb/lib/intel64/gcc4.8

WORKDIR ${CORE}/cvs/objects/build/linux/
RUN make clean
RUN make gcam -j 8

########################################
#
# cleanup build
#
########################################
WORKDIR /tmp
RUN rm -rf *.zip

########################################
#
# run gcam-core
#
########################################
WORKDIR ${CORE}
# ./gcam.exe -c configuration_ref.xml
