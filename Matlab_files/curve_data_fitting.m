%% fitting smooth curves to the data
exp_lb = 1; exp_lb_2 = 1;
exp_ub = 100;

CA_raw = [CA_1_low_l_cum,CA_1_low_l_sort(:,2),...
    CA_1_high_l_cum,CA_1_high_l_sort(:,2),...
    CA_1_low_h_cum,CA_1_low_h_sort(:,2),...
    CA_1_high_h_cum,CA_1_high_h_sort(:,2),...
    CA_2_low_l_cum,CA_2_low_l_sort(:,2),...
    CA_2_high_l_cum,CA_2_high_l_sort(:,2),...
    CA_2_low_h_cum,CA_2_low_h_sort(:,2),...
    CA_2_high_h_cum,CA_2_high_h_sort(:,2),...
    CA_3_low_l_cum,CA_3_low_l_sort(:,2),...
    CA_3_high_l_cum,CA_3_high_l_sort(:,2),...
    CA_3_low_h_cum,CA_3_low_h_sort(:,2),...
    CA_3_high_h_cum,CA_3_high_h_sort(:,2),...
    CA_4_low_l_cum,CA_4_low_l_sort(:,2),...
    CA_4_high_l_cum,CA_4_high_l_sort(:,2),...
    CA_4_low_h_cum,CA_4_low_h_sort(:,2),...
    CA_4_high_h_cum,CA_4_high_h_sort(:,2),...
    CA_5_low_l_cum,CA_5_low_l_sort(:,2),...
    CA_5_high_l_cum,CA_5_high_l_sort(:,2),...
    CA_5_low_h_cum,CA_5_low_h_sort(:,2),...
    CA_5_high_h_cum,CA_5_high_h_sort(:,2),...
    CA_6_low_l_cum,CA_6_low_l_sort(:,2),...
    CA_6_high_l_cum,CA_6_high_l_sort(:,2),...
    CA_6_low_h_cum,CA_6_low_h_sort(:,2),...
    CA_6_high_h_cum,CA_6_high_h_sort(:,2),...
    CA_7_low_l_cum,CA_7_low_l_sort(:,2),...
    CA_7_high_l_cum,CA_7_high_l_sort(:,2),...
    CA_7_low_h_cum,CA_7_low_h_sort(:,2),...
    CA_7_high_h_cum,CA_7_high_h_sort(:,2)];

CA_fval = zeros(1,size(CA_raw,2)/2);
CA_exp = CA_fval; CA_mid = CA_fval;
CA_fit = zeros(size(CA_raw));
CA_error = zeros(size(CA_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(CA_raw,2)/2;
if CA_raw(:,2*i-1) == 0;
else
    [CA_fval(i) CA_exp(i) CA_mid(i) CA_fit(:,2*i-1) CA_error(:,i)] = curve_fitting(CA_raw(:,2*i-1),CA_raw(:,2*i),x0,lb,ub);
    CA_fit(:,2*i) = CA_raw(:,2*i);
    if CA_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(CA_raw(:,2*i-1),CA_raw(:,2*i),x0,lb,ub);
        if fval < CA_fval(i);
            CA_fval(i) = fval; CA_exp(i) = exp; CA_mid(i) = mid;
            CA_fit(:,2*i-1) = fit;
        end
    end
end
end
% Reminder: low and high refer to energy, l and h refer to costs

CT_raw = [CT_1_low_l_cum,CT_1_low_l_sort(:,2),...
    CT_1_high_l_cum,CT_1_high_l_sort(:,2),...
    CT_1_low_h_cum,CT_1_low_h_sort(:,2),...
    CT_1_high_h_cum,CT_1_high_h_sort(:,2),...
    CT_2_low_l_cum,CT_2_low_l_sort(:,2),...
    CT_2_high_l_cum,CT_2_high_l_sort(:,2),...
    CT_2_low_h_cum,CT_2_low_h_sort(:,2),...
    CT_2_high_h_cum,CT_2_high_h_sort(:,2),...
    CT_3_low_l_cum,CT_3_low_l_sort(:,2),...
    CT_3_high_l_cum,CT_3_high_l_sort(:,2),...
    CT_3_low_h_cum,CT_3_low_h_sort(:,2),...
    CT_3_high_h_cum,CT_3_high_h_sort(:,2),...
    CT_4_low_l_cum,CT_4_low_l_sort(:,2),...
    CT_4_high_l_cum,CT_4_high_l_sort(:,2),...
    CT_4_low_h_cum,CT_4_low_h_sort(:,2),...
    CT_4_high_h_cum,CT_4_high_h_sort(:,2),...
    CT_5_low_l_cum,CT_5_low_l_sort(:,2),...
    CT_5_high_l_cum,CT_5_high_l_sort(:,2),...
    CT_5_low_h_cum,CT_5_low_h_sort(:,2),...
    CT_5_high_h_cum,CT_5_high_h_sort(:,2),...
    CT_6_low_l_cum,CT_6_low_l_sort(:,2),...
    CT_6_high_l_cum,CT_6_high_l_sort(:,2),...
    CT_6_low_h_cum,CT_6_low_h_sort(:,2),...
    CT_6_high_h_cum,CT_6_high_h_sort(:,2),...
    CT_7_low_l_cum,CT_7_low_l_sort(:,2),...
    CT_7_high_l_cum,CT_7_high_l_sort(:,2),...
    CT_7_low_h_cum,CT_7_low_h_sort(:,2),...
    CT_7_high_h_cum,CT_7_high_h_sort(:,2)];

CT_fval = zeros(1,size(CT_raw,2)/2);
CT_exp = CT_fval; CT_mid = CT_fval;
CT_fit = zeros(size(CT_raw));
CT_error = zeros(size(CT_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(CT_raw,2)/2;
if CT_raw(:,2*i-1) == 0;
else
    [CT_fval(i) CT_exp(i) CT_mid(i) CT_fit(:,2*i-1) CT_error(:,i)] = curve_fitting(CT_raw(:,2*i-1),CT_raw(:,2*i),x0,lb,ub);
    CT_fit(:,2*i) = CT_raw(:,2*i);
    if CT_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(CT_raw(:,2*i-1),CT_raw(:,2*i),x0,lb,ub);
        if fval < CT_fval(i);
            CT_fval(i) = fval; CT_exp(i) = exp; CT_mid(i) = mid;
            CT_fit(:,2*i-1) = fit;
        end
    end
end
end

DE_raw = [DE_1_low_l_cum,DE_1_low_l_sort(:,2),...
    DE_1_high_l_cum,DE_1_high_l_sort(:,2),...
    DE_1_low_h_cum,DE_1_low_h_sort(:,2),...
    DE_1_high_h_cum,DE_1_high_h_sort(:,2),...
    DE_2_low_l_cum,DE_2_low_l_sort(:,2),...
    DE_2_high_l_cum,DE_2_high_l_sort(:,2),...
    DE_2_low_h_cum,DE_2_low_h_sort(:,2),...
    DE_2_high_h_cum,DE_2_high_h_sort(:,2),...
    DE_3_low_l_cum,DE_3_low_l_sort(:,2),...
    DE_3_high_l_cum,DE_3_high_l_sort(:,2),...
    DE_3_low_h_cum,DE_3_low_h_sort(:,2),...
    DE_3_high_h_cum,DE_3_high_h_sort(:,2),...
    DE_4_low_l_cum,DE_4_low_l_sort(:,2),...
    DE_4_high_l_cum,DE_4_high_l_sort(:,2),...
    DE_4_low_h_cum,DE_4_low_h_sort(:,2),...
    DE_4_high_h_cum,DE_4_high_h_sort(:,2),...
    DE_5_low_l_cum,DE_5_low_l_sort(:,2),...
    DE_5_high_l_cum,DE_5_high_l_sort(:,2),...
    DE_5_low_h_cum,DE_5_low_h_sort(:,2),...
    DE_5_high_h_cum,DE_5_high_h_sort(:,2),...
    DE_6_low_l_cum,DE_6_low_l_sort(:,2),...
    DE_6_high_l_cum,DE_6_high_l_sort(:,2),...
    DE_6_low_h_cum,DE_6_low_h_sort(:,2),...
    DE_6_high_h_cum,DE_6_high_h_sort(:,2),...
    DE_7_low_l_cum,DE_7_low_l_sort(:,2),...
    DE_7_high_l_cum,DE_7_high_l_sort(:,2),...
    DE_7_low_h_cum,DE_7_low_h_sort(:,2),...
    DE_7_high_h_cum,DE_7_high_h_sort(:,2)];

DE_fval = zeros(1,size(DE_raw,2)/2);
DE_exp = DE_fval; DE_mid = DE_fval;
DE_fit = zeros(size(DE_raw));
DE_error = zeros(size(DE_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(DE_raw,2)/2;
if DE_raw(:,2*i-1) == 0;
else
    [DE_fval(i) DE_exp(i) DE_mid(i) DE_fit(:,2*i-1) DE_error(:,i)] = curve_fitting(DE_raw(:,2*i-1),DE_raw(:,2*i),x0,lb,ub);
    DE_fit(:,2*i) = DE_raw(:,2*i);
    if DE_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(DE_raw(:,2*i-1),DE_raw(:,2*i),x0,lb,ub);
        if fval < DE_fval(i);
            DE_fval(i) = fval; DE_exp(i) = exp; DE_mid(i) = mid;
            DE_fit(:,2*i-1) = fit;
        end
    end
end
end

GA_raw = [GA_1_low_l_cum,GA_1_low_l_sort(:,2),...
    GA_1_high_l_cum,GA_1_high_l_sort(:,2),...
    GA_1_low_h_cum,GA_1_low_h_sort(:,2),...
    GA_1_high_h_cum,GA_1_high_h_sort(:,2),...
    GA_2_low_l_cum,GA_2_low_l_sort(:,2),...
    GA_2_high_l_cum,GA_2_high_l_sort(:,2),...
    GA_2_low_h_cum,GA_2_low_h_sort(:,2),...
    GA_2_high_h_cum,GA_2_high_h_sort(:,2),...
    GA_3_low_l_cum,GA_3_low_l_sort(:,2),...
    GA_3_high_l_cum,GA_3_high_l_sort(:,2),...
    GA_3_low_h_cum,GA_3_low_h_sort(:,2),...
    GA_3_high_h_cum,GA_3_high_h_sort(:,2),...
    GA_4_low_l_cum,GA_4_low_l_sort(:,2),...
    GA_4_high_l_cum,GA_4_high_l_sort(:,2),...
    GA_4_low_h_cum,GA_4_low_h_sort(:,2),...
    GA_4_high_h_cum,GA_4_high_h_sort(:,2),...
    GA_5_low_l_cum,GA_5_low_l_sort(:,2),...
    GA_5_high_l_cum,GA_5_high_l_sort(:,2),...
    GA_5_low_h_cum,GA_5_low_h_sort(:,2),...
    GA_5_high_h_cum,GA_5_high_h_sort(:,2),...
    GA_6_low_l_cum,GA_6_low_l_sort(:,2),...
    GA_6_high_l_cum,GA_6_high_l_sort(:,2),...
    GA_6_low_h_cum,GA_6_low_h_sort(:,2),...
    GA_6_high_h_cum,GA_6_high_h_sort(:,2),...
    GA_7_low_l_cum,GA_7_low_l_sort(:,2),...
    GA_7_high_l_cum,GA_7_high_l_sort(:,2),...
    GA_7_low_h_cum,GA_7_low_h_sort(:,2),...
    GA_7_high_h_cum,GA_7_high_h_sort(:,2)];

GA_fval = zeros(1,size(GA_raw,2)/2);
GA_exp = GA_fval; GA_mid = GA_fval;
GA_fit = zeros(size(GA_raw));
GA_error = zeros(size(GA_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(GA_raw,2)/2;
if GA_raw(:,2*i-1) == 0;
else
    [GA_fval(i) GA_exp(i) GA_mid(i) GA_fit(:,2*i-1) GA_error(:,i)] = curve_fitting(GA_raw(:,2*i-1),GA_raw(:,2*i),x0,lb,ub);
    GA_fit(:,2*i) = GA_raw(:,2*i);
    if GA_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(GA_raw(:,2*i-1),GA_raw(:,2*i),x0,lb,ub);
        if fval < GA_fval(i);
            GA_fval(i) = fval; GA_exp(i) = exp; GA_mid(i) = mid;
            GA_fit(:,2*i-1) = fit;
        end
    end
end
end

HI_raw = [HI_1_low_l_cum,HI_1_low_l_sort(:,2),...
    HI_1_high_l_cum,HI_1_high_l_sort(:,2),...
    HI_1_low_h_cum,HI_1_low_h_sort(:,2),...
    HI_1_high_h_cum,HI_1_high_h_sort(:,2),...
    HI_2_low_l_cum,HI_2_low_l_sort(:,2),...
    HI_2_high_l_cum,HI_2_high_l_sort(:,2),...
    HI_2_low_h_cum,HI_2_low_h_sort(:,2),...
    HI_2_high_h_cum,HI_2_high_h_sort(:,2),...
    HI_3_low_l_cum,HI_3_low_l_sort(:,2),...
    HI_3_high_l_cum,HI_3_high_l_sort(:,2),...
    HI_3_low_h_cum,HI_3_low_h_sort(:,2),...
    HI_3_high_h_cum,HI_3_high_h_sort(:,2),...
    HI_4_low_l_cum,HI_4_low_l_sort(:,2),...
    HI_4_high_l_cum,HI_4_high_l_sort(:,2),...
    HI_4_low_h_cum,HI_4_low_h_sort(:,2),...
    HI_4_high_h_cum,HI_4_high_h_sort(:,2),...
    HI_5_low_l_cum,HI_5_low_l_sort(:,2),...
    HI_5_high_l_cum,HI_5_high_l_sort(:,2),...
    HI_5_low_h_cum,HI_5_low_h_sort(:,2),...
    HI_5_high_h_cum,HI_5_high_h_sort(:,2),...
    HI_6_low_l_cum,HI_6_low_l_sort(:,2),...
    HI_6_high_l_cum,HI_6_high_l_sort(:,2),...
    HI_6_low_h_cum,HI_6_low_h_sort(:,2),...
    HI_6_high_h_cum,HI_6_high_h_sort(:,2),...
    HI_7_low_l_cum,HI_7_low_l_sort(:,2),...
    HI_7_high_l_cum,HI_7_high_l_sort(:,2),...
    HI_7_low_h_cum,HI_7_low_h_sort(:,2),...
    HI_7_high_h_cum,HI_7_high_h_sort(:,2)];

HI_fval = zeros(1,size(HI_raw,2)/2);
HI_exp = HI_fval; HI_mid = HI_fval;
HI_fit = zeros(size(HI_raw));
HI_error = zeros(size(HI_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(HI_raw,2)/2;
if HI_raw(:,2*i-1) == 0;
else
    [HI_fval(i) HI_exp(i) HI_mid(i) HI_fit(:,2*i-1) HI_error(:,i)] = curve_fitting(HI_raw(:,2*i-1),HI_raw(:,2*i),x0,lb,ub);
    HI_fit(:,2*i) = HI_raw(:,2*i);
    if HI_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(HI_raw(:,2*i-1),HI_raw(:,2*i),x0,lb,ub);
        if fval < HI_fval(i);
            HI_fval(i) = fval; HI_exp(i) = exp; HI_mid(i) = mid;
            HI_fit(:,2*i-1) = fit;
        end
    end
end
end

IL_raw = [IL_1_low_l_cum,IL_1_low_l_sort(:,2),...
    IL_1_high_l_cum,IL_1_high_l_sort(:,2),...
    IL_1_low_h_cum,IL_1_low_h_sort(:,2),...
    IL_1_high_h_cum,IL_1_high_h_sort(:,2),...
    IL_2_low_l_cum,IL_2_low_l_sort(:,2),...
    IL_2_high_l_cum,IL_2_high_l_sort(:,2),...
    IL_2_low_h_cum,IL_2_low_h_sort(:,2),...
    IL_2_high_h_cum,IL_2_high_h_sort(:,2),...
    IL_3_low_l_cum,IL_3_low_l_sort(:,2),...
    IL_3_high_l_cum,IL_3_high_l_sort(:,2),...
    IL_3_low_h_cum,IL_3_low_h_sort(:,2),...
    IL_3_high_h_cum,IL_3_high_h_sort(:,2),...
    IL_4_low_l_cum,IL_4_low_l_sort(:,2),...
    IL_4_high_l_cum,IL_4_high_l_sort(:,2),...
    IL_4_low_h_cum,IL_4_low_h_sort(:,2),...
    IL_4_high_h_cum,IL_4_high_h_sort(:,2),...
    IL_5_low_l_cum,IL_5_low_l_sort(:,2),...
    IL_5_high_l_cum,IL_5_high_l_sort(:,2),...
    IL_5_low_h_cum,IL_5_low_h_sort(:,2),...
    IL_5_high_h_cum,IL_5_high_h_sort(:,2),...
    IL_6_low_l_cum,IL_6_low_l_sort(:,2),...
    IL_6_high_l_cum,IL_6_high_l_sort(:,2),...
    IL_6_low_h_cum,IL_6_low_h_sort(:,2),...
    IL_6_high_h_cum,IL_6_high_h_sort(:,2),...
    IL_7_low_l_cum,IL_7_low_l_sort(:,2),...
    IL_7_high_l_cum,IL_7_high_l_sort(:,2),...
    IL_7_low_h_cum,IL_7_low_h_sort(:,2),...
    IL_7_high_h_cum,IL_7_high_h_sort(:,2)];

IL_fval = zeros(1,size(IL_raw,2)/2);
IL_exp = IL_fval; IL_mid = IL_fval;
IL_fit = zeros(size(IL_raw));
IL_error = zeros(size(IL_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(IL_raw,2)/2;
if IL_raw(:,2*i-1) == 0;
else
    [IL_fval(i) IL_exp(i) IL_mid(i) IL_fit(:,2*i-1) IL_error(:,i)] = curve_fitting(IL_raw(:,2*i-1),IL_raw(:,2*i),x0,lb,ub);
    IL_fit(:,2*i) = IL_raw(:,2*i);
    if IL_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(IL_raw(:,2*i-1),IL_raw(:,2*i),x0,lb,ub);
        if fval < IL_fval(i);
            IL_fval(i) = fval; IL_exp(i) = exp; IL_mid(i) = mid;
            IL_fit(:,2*i-1) = fit;
        end
    end
end
end

IN_raw = [IN_1_low_l_cum,IN_1_low_l_sort(:,2),...
    IN_1_high_l_cum,IN_1_high_l_sort(:,2),...
    IN_1_low_h_cum,IN_1_low_h_sort(:,2),...
    IN_1_high_h_cum,IN_1_high_h_sort(:,2),...
    IN_2_low_l_cum,IN_2_low_l_sort(:,2),...
    IN_2_high_l_cum,IN_2_high_l_sort(:,2),...
    IN_2_low_h_cum,IN_2_low_h_sort(:,2),...
    IN_2_high_h_cum,IN_2_high_h_sort(:,2),...
    IN_3_low_l_cum,IN_3_low_l_sort(:,2),...
    IN_3_high_l_cum,IN_3_high_l_sort(:,2),...
    IN_3_low_h_cum,IN_3_low_h_sort(:,2),...
    IN_3_high_h_cum,IN_3_high_h_sort(:,2),...
    IN_4_low_l_cum,IN_4_low_l_sort(:,2),...
    IN_4_high_l_cum,IN_4_high_l_sort(:,2),...
    IN_4_low_h_cum,IN_4_low_h_sort(:,2),...
    IN_4_high_h_cum,IN_4_high_h_sort(:,2),...
    IN_5_low_l_cum,IN_5_low_l_sort(:,2),...
    IN_5_high_l_cum,IN_5_high_l_sort(:,2),...
    IN_5_low_h_cum,IN_5_low_h_sort(:,2),...
    IN_5_high_h_cum,IN_5_high_h_sort(:,2),...
    IN_6_low_l_cum,IN_6_low_l_sort(:,2),...
    IN_6_high_l_cum,IN_6_high_l_sort(:,2),...
    IN_6_low_h_cum,IN_6_low_h_sort(:,2),...
    IN_6_high_h_cum,IN_6_high_h_sort(:,2),...
    IN_7_low_l_cum,IN_7_low_l_sort(:,2),...
    IN_7_high_l_cum,IN_7_high_l_sort(:,2),...
    IN_7_low_h_cum,IN_7_low_h_sort(:,2),...
    IN_7_high_h_cum,IN_7_high_h_sort(:,2)];

IN_fval = zeros(1,size(IN_raw,2)/2);
IN_exp = IN_fval; IN_mid = IN_fval;
IN_fit = zeros(size(IN_raw));
IN_error = zeros(size(IN_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(IN_raw,2)/2;
if IN_raw(:,2*i-1) == 0;
else
    [IN_fval(i) IN_exp(i) IN_mid(i) IN_fit(:,2*i-1) IN_error(:,i)] = curve_fitting(IN_raw(:,2*i-1),IN_raw(:,2*i),x0,lb,ub);
    IN_fit(:,2*i) = IN_raw(:,2*i);
    if IN_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(IN_raw(:,2*i-1),IN_raw(:,2*i),x0,lb,ub);
        if fval < IN_fval(i);
            IN_fval(i) = fval; IN_exp(i) = exp; IN_mid(i) = mid;
            IN_fit(:,2*i-1) = fit;
        end
    end
end
end

LA_raw = [LA_1_low_l_cum,LA_1_low_l_sort(:,2),...
    LA_1_high_l_cum,LA_1_high_l_sort(:,2),...
    LA_1_low_h_cum,LA_1_low_h_sort(:,2),...
    LA_1_high_h_cum,LA_1_high_h_sort(:,2),...
    LA_2_low_l_cum,LA_2_low_l_sort(:,2),...
    LA_2_high_l_cum,LA_2_high_l_sort(:,2),...
    LA_2_low_h_cum,LA_2_low_h_sort(:,2),...
    LA_2_high_h_cum,LA_2_high_h_sort(:,2),...
    LA_3_low_l_cum,LA_3_low_l_sort(:,2),...
    LA_3_high_l_cum,LA_3_high_l_sort(:,2),...
    LA_3_low_h_cum,LA_3_low_h_sort(:,2),...
    LA_3_high_h_cum,LA_3_high_h_sort(:,2),...
    LA_4_low_l_cum,LA_4_low_l_sort(:,2),...
    LA_4_high_l_cum,LA_4_high_l_sort(:,2),...
    LA_4_low_h_cum,LA_4_low_h_sort(:,2),...
    LA_4_high_h_cum,LA_4_high_h_sort(:,2),...
    LA_5_low_l_cum,LA_5_low_l_sort(:,2),...
    LA_5_high_l_cum,LA_5_high_l_sort(:,2),...
    LA_5_low_h_cum,LA_5_low_h_sort(:,2),...
    LA_5_high_h_cum,LA_5_high_h_sort(:,2),...
    LA_6_low_l_cum,LA_6_low_l_sort(:,2),...
    LA_6_high_l_cum,LA_6_high_l_sort(:,2),...
    LA_6_low_h_cum,LA_6_low_h_sort(:,2),...
    LA_6_high_h_cum,LA_6_high_h_sort(:,2),...
    LA_7_low_l_cum,LA_7_low_l_sort(:,2),...
    LA_7_high_l_cum,LA_7_high_l_sort(:,2),...
    LA_7_low_h_cum,LA_7_low_h_sort(:,2),...
    LA_7_high_h_cum,LA_7_high_h_sort(:,2)];

LA_fval = zeros(1,size(LA_raw,2)/2);
LA_exp = LA_fval; LA_mid = LA_fval;
LA_fit = zeros(size(LA_raw));
LA_error = zeros(size(LA_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(LA_raw,2)/2;
if LA_raw(:,2*i-1) == 0;
else
    [LA_fval(i) LA_exp(i) LA_mid(i) LA_fit(:,2*i-1) LA_error(:,i)] = curve_fitting(LA_raw(:,2*i-1),LA_raw(:,2*i),x0,lb,ub);
    LA_fit(:,2*i) = LA_raw(:,2*i);
    if LA_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(LA_raw(:,2*i-1),LA_raw(:,2*i),x0,lb,ub);
        if fval < LA_fval(i);
            LA_fval(i) = fval; LA_exp(i) = exp; LA_mid(i) = mid;
            LA_fit(:,2*i-1) = fit;
        end
    end
end
end

ME_raw = [ME_1_low_l_cum,ME_1_low_l_sort(:,2),...
    ME_1_high_l_cum,ME_1_high_l_sort(:,2),...
    ME_1_low_h_cum,ME_1_low_h_sort(:,2),...
    ME_1_high_h_cum,ME_1_high_h_sort(:,2),...
    ME_2_low_l_cum,ME_2_low_l_sort(:,2),...
    ME_2_high_l_cum,ME_2_high_l_sort(:,2),...
    ME_2_low_h_cum,ME_2_low_h_sort(:,2),...
    ME_2_high_h_cum,ME_2_high_h_sort(:,2),...
    ME_3_low_l_cum,ME_3_low_l_sort(:,2),...
    ME_3_high_l_cum,ME_3_high_l_sort(:,2),...
    ME_3_low_h_cum,ME_3_low_h_sort(:,2),...
    ME_3_high_h_cum,ME_3_high_h_sort(:,2),...
    ME_4_low_l_cum,ME_4_low_l_sort(:,2),...
    ME_4_high_l_cum,ME_4_high_l_sort(:,2),...
    ME_4_low_h_cum,ME_4_low_h_sort(:,2),...
    ME_4_high_h_cum,ME_4_high_h_sort(:,2),...
    ME_5_low_l_cum,ME_5_low_l_sort(:,2),...
    ME_5_high_l_cum,ME_5_high_l_sort(:,2),...
    ME_5_low_h_cum,ME_5_low_h_sort(:,2),...
    ME_5_high_h_cum,ME_5_high_h_sort(:,2),...
    ME_6_low_l_cum,ME_6_low_l_sort(:,2),...
    ME_6_high_l_cum,ME_6_high_l_sort(:,2),...
    ME_6_low_h_cum,ME_6_low_h_sort(:,2),...
    ME_6_high_h_cum,ME_6_high_h_sort(:,2),...
    ME_7_low_l_cum,ME_7_low_l_sort(:,2),...
    ME_7_high_l_cum,ME_7_high_l_sort(:,2),...
    ME_7_low_h_cum,ME_7_low_h_sort(:,2),...
    ME_7_high_h_cum,ME_7_high_h_sort(:,2)];

ME_fval = zeros(1,size(ME_raw,2)/2);
ME_exp = ME_fval; ME_mid = ME_fval;
ME_fit = zeros(size(ME_raw));
ME_error = zeros(size(ME_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(ME_raw,2)/2;
if ME_raw(:,2*i-1) == 0;
else
    [ME_fval(i) ME_exp(i) ME_mid(i) ME_fit(:,2*i-1) ME_error(:,i)] = curve_fitting(ME_raw(:,2*i-1),ME_raw(:,2*i),x0,lb,ub);
    ME_fit(:,2*i) = ME_raw(:,2*i);
    if ME_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(ME_raw(:,2*i-1),ME_raw(:,2*i),x0,lb,ub);
        if fval < ME_fval(i);
            ME_fval(i) = fval; ME_exp(i) = exp; ME_mid(i) = mid;
            ME_fit(:,2*i-1) = fit;
        end
    end
end
end

MA_raw = [MA_1_low_l_cum,MA_1_low_l_sort(:,2),...
    MA_1_high_l_cum,MA_1_high_l_sort(:,2),...
    MA_1_low_h_cum,MA_1_low_h_sort(:,2),...
    MA_1_high_h_cum,MA_1_high_h_sort(:,2),...
    MA_2_low_l_cum,MA_2_low_l_sort(:,2),...
    MA_2_high_l_cum,MA_2_high_l_sort(:,2),...
    MA_2_low_h_cum,MA_2_low_h_sort(:,2),...
    MA_2_high_h_cum,MA_2_high_h_sort(:,2),...
    MA_3_low_l_cum,MA_3_low_l_sort(:,2),...
    MA_3_high_l_cum,MA_3_high_l_sort(:,2),...
    MA_3_low_h_cum,MA_3_low_h_sort(:,2),...
    MA_3_high_h_cum,MA_3_high_h_sort(:,2),...
    MA_4_low_l_cum,MA_4_low_l_sort(:,2),...
    MA_4_high_l_cum,MA_4_high_l_sort(:,2),...
    MA_4_low_h_cum,MA_4_low_h_sort(:,2),...
    MA_4_high_h_cum,MA_4_high_h_sort(:,2),...
    MA_5_low_l_cum,MA_5_low_l_sort(:,2),...
    MA_5_high_l_cum,MA_5_high_l_sort(:,2),...
    MA_5_low_h_cum,MA_5_low_h_sort(:,2),...
    MA_5_high_h_cum,MA_5_high_h_sort(:,2),...
    MA_6_low_l_cum,MA_6_low_l_sort(:,2),...
    MA_6_high_l_cum,MA_6_high_l_sort(:,2),...
    MA_6_low_h_cum,MA_6_low_h_sort(:,2),...
    MA_6_high_h_cum,MA_6_high_h_sort(:,2),...
    MA_7_low_l_cum,MA_7_low_l_sort(:,2),...
    MA_7_high_l_cum,MA_7_high_l_sort(:,2),...
    MA_7_low_h_cum,MA_7_low_h_sort(:,2),...
    MA_7_high_h_cum,MA_7_high_h_sort(:,2)];

MA_fval = zeros(1,size(MA_raw,2)/2);
MA_exp = MA_fval; MA_mid = MA_fval;
MA_fit = zeros(size(MA_raw));
MA_error = zeros(size(MA_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(MA_raw,2)/2;
if MA_raw(:,2*i-1) == 0;
else
    [MA_fval(i) MA_exp(i) MA_mid(i) MA_fit(:,2*i-1) MA_error(:,i)] = curve_fitting(MA_raw(:,2*i-1),MA_raw(:,2*i),x0,lb,ub);
    MA_fit(:,2*i) = MA_raw(:,2*i);
    if MA_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(MA_raw(:,2*i-1),MA_raw(:,2*i),x0,lb,ub);
        if fval < MA_fval(i);
            MA_fval(i) = fval; MA_exp(i) = exp; MA_mid(i) = mid;
            MA_fit(:,2*i-1) = fit;
        end
    end
end
end

MD_raw = [MD_1_low_l_cum,MD_1_low_l_sort(:,2),...
    MD_1_high_l_cum,MD_1_high_l_sort(:,2),...
    MD_1_low_h_cum,MD_1_low_h_sort(:,2),...
    MD_1_high_h_cum,MD_1_high_h_sort(:,2),...
    MD_2_low_l_cum,MD_2_low_l_sort(:,2),...
    MD_2_high_l_cum,MD_2_high_l_sort(:,2),...
    MD_2_low_h_cum,MD_2_low_h_sort(:,2),...
    MD_2_high_h_cum,MD_2_high_h_sort(:,2),...
    MD_3_low_l_cum,MD_3_low_l_sort(:,2),...
    MD_3_high_l_cum,MD_3_high_l_sort(:,2),...
    MD_3_low_h_cum,MD_3_low_h_sort(:,2),...
    MD_3_high_h_cum,MD_3_high_h_sort(:,2),...
    MD_4_low_l_cum,MD_4_low_l_sort(:,2),...
    MD_4_high_l_cum,MD_4_high_l_sort(:,2),...
    MD_4_low_h_cum,MD_4_low_h_sort(:,2),...
    MD_4_high_h_cum,MD_4_high_h_sort(:,2),...
    MD_5_low_l_cum,MD_5_low_l_sort(:,2),...
    MD_5_high_l_cum,MD_5_high_l_sort(:,2),...
    MD_5_low_h_cum,MD_5_low_h_sort(:,2),...
    MD_5_high_h_cum,MD_5_high_h_sort(:,2),...
    MD_6_low_l_cum,MD_6_low_l_sort(:,2),...
    MD_6_high_l_cum,MD_6_high_l_sort(:,2),...
    MD_6_low_h_cum,MD_6_low_h_sort(:,2),...
    MD_6_high_h_cum,MD_6_high_h_sort(:,2),...
    MD_7_low_l_cum,MD_7_low_l_sort(:,2),...
    MD_7_high_l_cum,MD_7_high_l_sort(:,2),...
    MD_7_low_h_cum,MD_7_low_h_sort(:,2),...
    MD_7_high_h_cum,MD_7_high_h_sort(:,2)];

MD_fval = zeros(1,size(MD_raw,2)/2);
MD_exp = MD_fval; MD_mid = MD_fval;
MD_fit = zeros(size(MD_raw));
MD_error = zeros(size(MD_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(MD_raw,2)/2;
if MD_raw(:,2*i-1) == 0;
else
    [MD_fval(i) MD_exp(i) MD_mid(i) MD_fit(:,2*i-1) MD_error(:,i)] = curve_fitting(MD_raw(:,2*i-1),MD_raw(:,2*i),x0,lb,ub);
    MD_fit(:,2*i) = MD_raw(:,2*i);
    if MD_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(MD_raw(:,2*i-1),MD_raw(:,2*i),x0,lb,ub);
        if fval < MD_fval(i);
            MD_fval(i) = fval; MD_exp(i) = exp; MD_mid(i) = mid;
            MD_fit(:,2*i-1) = fit;
        end
    end
end
end

MI_raw = [MI_1_low_l_cum,MI_1_low_l_sort(:,2),...
    MI_1_high_l_cum,MI_1_high_l_sort(:,2),...
    MI_1_low_h_cum,MI_1_low_h_sort(:,2),...
    MI_1_high_h_cum,MI_1_high_h_sort(:,2),...
    MI_2_low_l_cum,MI_2_low_l_sort(:,2),...
    MI_2_high_l_cum,MI_2_high_l_sort(:,2),...
    MI_2_low_h_cum,MI_2_low_h_sort(:,2),...
    MI_2_high_h_cum,MI_2_high_h_sort(:,2),...
    MI_3_low_l_cum,MI_3_low_l_sort(:,2),...
    MI_3_high_l_cum,MI_3_high_l_sort(:,2),...
    MI_3_low_h_cum,MI_3_low_h_sort(:,2),...
    MI_3_high_h_cum,MI_3_high_h_sort(:,2),...
    MI_4_low_l_cum,MI_4_low_l_sort(:,2),...
    MI_4_high_l_cum,MI_4_high_l_sort(:,2),...
    MI_4_low_h_cum,MI_4_low_h_sort(:,2),...
    MI_4_high_h_cum,MI_4_high_h_sort(:,2),...
    MI_5_low_l_cum,MI_5_low_l_sort(:,2),...
    MI_5_high_l_cum,MI_5_high_l_sort(:,2),...
    MI_5_low_h_cum,MI_5_low_h_sort(:,2),...
    MI_5_high_h_cum,MI_5_high_h_sort(:,2),...
    MI_6_low_l_cum,MI_6_low_l_sort(:,2),...
    MI_6_high_l_cum,MI_6_high_l_sort(:,2),...
    MI_6_low_h_cum,MI_6_low_h_sort(:,2),...
    MI_6_high_h_cum,MI_6_high_h_sort(:,2),...
    MI_7_low_l_cum,MI_7_low_l_sort(:,2),...
    MI_7_high_l_cum,MI_7_high_l_sort(:,2),...
    MI_7_low_h_cum,MI_7_low_h_sort(:,2),...
    MI_7_high_h_cum,MI_7_high_h_sort(:,2)];

MI_fval = zeros(1,size(MI_raw,2)/2);
MI_exp = MI_fval; MI_mid = MI_fval;
MI_fit = zeros(size(MI_raw));
MI_error = zeros(size(MI_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(MI_raw,2)/2;
if MI_raw(:,2*i-1) == 0;
else
    [MI_fval(i) MI_exp(i) MI_mid(i) MI_fit(:,2*i-1) MI_error(:,i)] = curve_fitting(MI_raw(:,2*i-1),MI_raw(:,2*i),x0,lb,ub);
    MI_fit(:,2*i) = MI_raw(:,2*i);
    if MI_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(MI_raw(:,2*i-1),MI_raw(:,2*i),x0,lb,ub);
        if fval < MI_fval(i);
            MI_fval(i) = fval; MI_exp(i) = exp; MI_mid(i) = mid;
            MI_fit(:,2*i-1) = fit;
        end
    end
end
end

MN_raw = [MN_1_low_l_cum,MN_1_low_l_sort(:,2),...
    MN_1_high_l_cum,MN_1_high_l_sort(:,2),...
    MN_1_low_h_cum,MN_1_low_h_sort(:,2),...
    MN_1_high_h_cum,MN_1_high_h_sort(:,2),...
    MN_2_low_l_cum,MN_2_low_l_sort(:,2),...
    MN_2_high_l_cum,MN_2_high_l_sort(:,2),...
    MN_2_low_h_cum,MN_2_low_h_sort(:,2),...
    MN_2_high_h_cum,MN_2_high_h_sort(:,2),...
    MN_3_low_l_cum,MN_3_low_l_sort(:,2),...
    MN_3_high_l_cum,MN_3_high_l_sort(:,2),...
    MN_3_low_h_cum,MN_3_low_h_sort(:,2),...
    MN_3_high_h_cum,MN_3_high_h_sort(:,2),...
    MN_4_low_l_cum,MN_4_low_l_sort(:,2),...
    MN_4_high_l_cum,MN_4_high_l_sort(:,2),...
    MN_4_low_h_cum,MN_4_low_h_sort(:,2),...
    MN_4_high_h_cum,MN_4_high_h_sort(:,2),...
    MN_5_low_l_cum,MN_5_low_l_sort(:,2),...
    MN_5_high_l_cum,MN_5_high_l_sort(:,2),...
    MN_5_low_h_cum,MN_5_low_h_sort(:,2),...
    MN_5_high_h_cum,MN_5_high_h_sort(:,2),...
    MN_6_low_l_cum,MN_6_low_l_sort(:,2),...
    MN_6_high_l_cum,MN_6_high_l_sort(:,2),...
    MN_6_low_h_cum,MN_6_low_h_sort(:,2),...
    MN_6_high_h_cum,MN_6_high_h_sort(:,2),...
    MN_7_low_l_cum,MN_7_low_l_sort(:,2),...
    MN_7_high_l_cum,MN_7_high_l_sort(:,2),...
    MN_7_low_h_cum,MN_7_low_h_sort(:,2),...
    MN_7_high_h_cum,MN_7_high_h_sort(:,2)];

MN_fval = zeros(1,size(MN_raw,2)/2);
MN_exp = MN_fval; MN_mid = MN_fval;
MN_fit = zeros(size(MN_raw));
MN_error = zeros(size(MN_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(MN_raw,2)/2;
if MN_raw(:,2*i-1) == 0;
else
    [MN_fval(i) MN_exp(i) MN_mid(i) MN_fit(:,2*i-1) MN_error(:,i)] = curve_fitting(MN_raw(:,2*i-1),MN_raw(:,2*i),x0,lb,ub);
    MN_fit(:,2*i) = MN_raw(:,2*i);
    if MN_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(MN_raw(:,2*i-1),MN_raw(:,2*i),x0,lb,ub);
        if fval < MN_fval(i);
            MN_fval(i) = fval; MN_exp(i) = exp; MN_mid(i) = mid;
            MN_fit(:,2*i-1) = fit;
        end
    end
end
end

NH_raw = [NH_1_low_l_cum,NH_1_low_l_sort(:,2),...
    NH_1_high_l_cum,NH_1_high_l_sort(:,2),...
    NH_1_low_h_cum,NH_1_low_h_sort(:,2),...
    NH_1_high_h_cum,NH_1_high_h_sort(:,2),...
    NH_2_low_l_cum,NH_2_low_l_sort(:,2),...
    NH_2_high_l_cum,NH_2_high_l_sort(:,2),...
    NH_2_low_h_cum,NH_2_low_h_sort(:,2),...
    NH_2_high_h_cum,NH_2_high_h_sort(:,2),...
    NH_3_low_l_cum,NH_3_low_l_sort(:,2),...
    NH_3_high_l_cum,NH_3_high_l_sort(:,2),...
    NH_3_low_h_cum,NH_3_low_h_sort(:,2),...
    NH_3_high_h_cum,NH_3_high_h_sort(:,2),...
    NH_4_low_l_cum,NH_4_low_l_sort(:,2),...
    NH_4_high_l_cum,NH_4_high_l_sort(:,2),...
    NH_4_low_h_cum,NH_4_low_h_sort(:,2),...
    NH_4_high_h_cum,NH_4_high_h_sort(:,2),...
    NH_5_low_l_cum,NH_5_low_l_sort(:,2),...
    NH_5_high_l_cum,NH_5_high_l_sort(:,2),...
    NH_5_low_h_cum,NH_5_low_h_sort(:,2),...
    NH_5_high_h_cum,NH_5_high_h_sort(:,2),...
    NH_6_low_l_cum,NH_6_low_l_sort(:,2),...
    NH_6_high_l_cum,NH_6_high_l_sort(:,2),...
    NH_6_low_h_cum,NH_6_low_h_sort(:,2),...
    NH_6_high_h_cum,NH_6_high_h_sort(:,2),...
    NH_7_low_l_cum,NH_7_low_l_sort(:,2),...
    NH_7_high_l_cum,NH_7_high_l_sort(:,2),...
    NH_7_low_h_cum,NH_7_low_h_sort(:,2),...
    NH_7_high_h_cum,NH_7_high_h_sort(:,2)];

NH_fval = zeros(1,size(NH_raw,2)/2);
NH_exp = NH_fval; NH_mid = NH_fval;
NH_fit = zeros(size(NH_raw));
NH_error = zeros(size(NH_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(NH_raw,2)/2;
if NH_raw(:,2*i-1) == 0;
else
    [NH_fval(i) NH_exp(i) NH_mid(i) NH_fit(:,2*i-1) NH_error(:,i)] = curve_fitting(NH_raw(:,2*i-1),NH_raw(:,2*i),x0,lb,ub);
    NH_fit(:,2*i) = NH_raw(:,2*i);
    if NH_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(NH_raw(:,2*i-1),NH_raw(:,2*i),x0,lb,ub);
        if fval < NH_fval(i);
            NH_fval(i) = fval; NH_exp(i) = exp; NH_mid(i) = mid;
            NH_fit(:,2*i-1) = fit;
        end
    end
end
end

NJ_raw = [NJ_1_low_l_cum,NJ_1_low_l_sort(:,2),...
    NJ_1_high_l_cum,NJ_1_high_l_sort(:,2),...
    NJ_1_low_h_cum,NJ_1_low_h_sort(:,2),...
    NJ_1_high_h_cum,NJ_1_high_h_sort(:,2),...
    NJ_2_low_l_cum,NJ_2_low_l_sort(:,2),...
    NJ_2_high_l_cum,NJ_2_high_l_sort(:,2),...
    NJ_2_low_h_cum,NJ_2_low_h_sort(:,2),...
    NJ_2_high_h_cum,NJ_2_high_h_sort(:,2),...
    NJ_3_low_l_cum,NJ_3_low_l_sort(:,2),...
    NJ_3_high_l_cum,NJ_3_high_l_sort(:,2),...
    NJ_3_low_h_cum,NJ_3_low_h_sort(:,2),...
    NJ_3_high_h_cum,NJ_3_high_h_sort(:,2),...
    NJ_4_low_l_cum,NJ_4_low_l_sort(:,2),...
    NJ_4_high_l_cum,NJ_4_high_l_sort(:,2),...
    NJ_4_low_h_cum,NJ_4_low_h_sort(:,2),...
    NJ_4_high_h_cum,NJ_4_high_h_sort(:,2),...
    NJ_5_low_l_cum,NJ_5_low_l_sort(:,2),...
    NJ_5_high_l_cum,NJ_5_high_l_sort(:,2),...
    NJ_5_low_h_cum,NJ_5_low_h_sort(:,2),...
    NJ_5_high_h_cum,NJ_5_high_h_sort(:,2),...
    NJ_6_low_l_cum,NJ_6_low_l_sort(:,2),...
    NJ_6_high_l_cum,NJ_6_high_l_sort(:,2),...
    NJ_6_low_h_cum,NJ_6_low_h_sort(:,2),...
    NJ_6_high_h_cum,NJ_6_high_h_sort(:,2),...
    NJ_7_low_l_cum,NJ_7_low_l_sort(:,2),...
    NJ_7_high_l_cum,NJ_7_high_l_sort(:,2),...
    NJ_7_low_h_cum,NJ_7_low_h_sort(:,2),...
    NJ_7_high_h_cum,NJ_7_high_h_sort(:,2)];

NJ_fval = zeros(1,size(NJ_raw,2)/2);
NJ_exp = NJ_fval; NJ_mid = NJ_fval;
NJ_fit = zeros(size(NJ_raw));
NJ_error = zeros(size(NJ_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(NJ_raw,2)/2;
if NJ_raw(:,2*i-1) == 0;
else
    [NJ_fval(i) NJ_exp(i) NJ_mid(i) NJ_fit(:,2*i-1) NJ_error(:,i)] = curve_fitting(NJ_raw(:,2*i-1),NJ_raw(:,2*i),x0,lb,ub);
    NJ_fit(:,2*i) = NJ_raw(:,2*i);
    if NJ_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(NJ_raw(:,2*i-1),NJ_raw(:,2*i),x0,lb,ub);
        if fval < NJ_fval(i);
            NJ_fval(i) = fval; NJ_exp(i) = exp; NJ_mid(i) = mid;
            NJ_fit(:,2*i-1) = fit;
        end
    end
end
end

NY_raw = [NY_1_low_l_cum,NY_1_low_l_sort(:,2),...
    NY_1_high_l_cum,NY_1_high_l_sort(:,2),...
    NY_1_low_h_cum,NY_1_low_h_sort(:,2),...
    NY_1_high_h_cum,NY_1_high_h_sort(:,2),...
    NY_2_low_l_cum,NY_2_low_l_sort(:,2),...
    NY_2_high_l_cum,NY_2_high_l_sort(:,2),...
    NY_2_low_h_cum,NY_2_low_h_sort(:,2),...
    NY_2_high_h_cum,NY_2_high_h_sort(:,2),...
    NY_3_low_l_cum,NY_3_low_l_sort(:,2),...
    NY_3_high_l_cum,NY_3_high_l_sort(:,2),...
    NY_3_low_h_cum,NY_3_low_h_sort(:,2),...
    NY_3_high_h_cum,NY_3_high_h_sort(:,2),...
    NY_4_low_l_cum,NY_4_low_l_sort(:,2),...
    NY_4_high_l_cum,NY_4_high_l_sort(:,2),...
    NY_4_low_h_cum,NY_4_low_h_sort(:,2),...
    NY_4_high_h_cum,NY_4_high_h_sort(:,2),...
    NY_5_low_l_cum,NY_5_low_l_sort(:,2),...
    NY_5_high_l_cum,NY_5_high_l_sort(:,2),...
    NY_5_low_h_cum,NY_5_low_h_sort(:,2),...
    NY_5_high_h_cum,NY_5_high_h_sort(:,2),...
    NY_6_low_l_cum,NY_6_low_l_sort(:,2),...
    NY_6_high_l_cum,NY_6_high_l_sort(:,2),...
    NY_6_low_h_cum,NY_6_low_h_sort(:,2),...
    NY_6_high_h_cum,NY_6_high_h_sort(:,2),...
    NY_7_low_l_cum,NY_7_low_l_sort(:,2),...
    NY_7_high_l_cum,NY_7_high_l_sort(:,2),...
    NY_7_low_h_cum,NY_7_low_h_sort(:,2),...
    NY_7_high_h_cum,NY_7_high_h_sort(:,2)];

NY_fval = zeros(1,size(NY_raw,2)/2);
NY_exp = NY_fval; NY_mid = NY_fval;
NY_fit = zeros(size(NY_raw));
NY_error = zeros(size(NY_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(NY_raw,2)/2;
if NY_raw(:,2*i-1) == 0;
else
    [NY_fval(i) NY_exp(i) NY_mid(i) NY_fit(:,2*i-1) NY_error(:,i)] = curve_fitting(NY_raw(:,2*i-1),NY_raw(:,2*i),x0,lb,ub);
    NY_fit(:,2*i) = NY_raw(:,2*i);
    if NY_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(NY_raw(:,2*i-1),NY_raw(:,2*i),x0,lb,ub);
        if fval < NY_fval(i);
            NY_fval(i) = fval; NY_exp(i) = exp; NY_mid(i) = mid;
            NY_fit(:,2*i-1) = fit;
        end
    end
end
end

NC_raw = [NC_1_low_l_cum,NC_1_low_l_sort(:,2),...
    NC_1_high_l_cum,NC_1_high_l_sort(:,2),...
    NC_1_low_h_cum,NC_1_low_h_sort(:,2),...
    NC_1_high_h_cum,NC_1_high_h_sort(:,2),...
    NC_2_low_l_cum,NC_2_low_l_sort(:,2),...
    NC_2_high_l_cum,NC_2_high_l_sort(:,2),...
    NC_2_low_h_cum,NC_2_low_h_sort(:,2),...
    NC_2_high_h_cum,NC_2_high_h_sort(:,2),...
    NC_3_low_l_cum,NC_3_low_l_sort(:,2),...
    NC_3_high_l_cum,NC_3_high_l_sort(:,2),...
    NC_3_low_h_cum,NC_3_low_h_sort(:,2),...
    NC_3_high_h_cum,NC_3_high_h_sort(:,2),...
    NC_4_low_l_cum,NC_4_low_l_sort(:,2),...
    NC_4_high_l_cum,NC_4_high_l_sort(:,2),...
    NC_4_low_h_cum,NC_4_low_h_sort(:,2),...
    NC_4_high_h_cum,NC_4_high_h_sort(:,2),...
    NC_5_low_l_cum,NC_5_low_l_sort(:,2),...
    NC_5_high_l_cum,NC_5_high_l_sort(:,2),...
    NC_5_low_h_cum,NC_5_low_h_sort(:,2),...
    NC_5_high_h_cum,NC_5_high_h_sort(:,2),...
    NC_6_low_l_cum,NC_6_low_l_sort(:,2),...
    NC_6_high_l_cum,NC_6_high_l_sort(:,2),...
    NC_6_low_h_cum,NC_6_low_h_sort(:,2),...
    NC_6_high_h_cum,NC_6_high_h_sort(:,2),...
    NC_7_low_l_cum,NC_7_low_l_sort(:,2),...
    NC_7_high_l_cum,NC_7_high_l_sort(:,2),...
    NC_7_low_h_cum,NC_7_low_h_sort(:,2),...
    NC_7_high_h_cum,NC_7_high_h_sort(:,2)];

NC_fval = zeros(1,size(NC_raw,2)/2);
NC_exp = NC_fval; NC_mid = NC_fval;
NC_fit = zeros(size(NC_raw));
NC_error = zeros(size(NC_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(NC_raw,2)/2;
if NC_raw(:,2*i-1) == 0;
else
    [NC_fval(i) NC_exp(i) NC_mid(i) NC_fit(:,2*i-1) NC_error(:,i)] = curve_fitting(NC_raw(:,2*i-1),NC_raw(:,2*i),x0,lb,ub);
    NC_fit(:,2*i) = NC_raw(:,2*i);
    if NC_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(NC_raw(:,2*i-1),NC_raw(:,2*i),x0,lb,ub);
        if fval < NC_fval(i);
            NC_fval(i) = fval; NC_exp(i) = exp; NC_mid(i) = mid;
            NC_fit(:,2*i-1) = fit;
        end
    end
end
end

OH_raw = [OH_1_low_l_cum,OH_1_low_l_sort(:,2),...
    OH_1_high_l_cum,OH_1_high_l_sort(:,2),...
    OH_1_low_h_cum,OH_1_low_h_sort(:,2),...
    OH_1_high_h_cum,OH_1_high_h_sort(:,2),...
    OH_2_low_l_cum,OH_2_low_l_sort(:,2),...
    OH_2_high_l_cum,OH_2_high_l_sort(:,2),...
    OH_2_low_h_cum,OH_2_low_h_sort(:,2),...
    OH_2_high_h_cum,OH_2_high_h_sort(:,2),...
    OH_3_low_l_cum,OH_3_low_l_sort(:,2),...
    OH_3_high_l_cum,OH_3_high_l_sort(:,2),...
    OH_3_low_h_cum,OH_3_low_h_sort(:,2),...
    OH_3_high_h_cum,OH_3_high_h_sort(:,2),...
    OH_4_low_l_cum,OH_4_low_l_sort(:,2),...
    OH_4_high_l_cum,OH_4_high_l_sort(:,2),...
    OH_4_low_h_cum,OH_4_low_h_sort(:,2),...
    OH_4_high_h_cum,OH_4_high_h_sort(:,2),...
    OH_5_low_l_cum,OH_5_low_l_sort(:,2),...
    OH_5_high_l_cum,OH_5_high_l_sort(:,2),...
    OH_5_low_h_cum,OH_5_low_h_sort(:,2),...
    OH_5_high_h_cum,OH_5_high_h_sort(:,2),...
    OH_6_low_l_cum,OH_6_low_l_sort(:,2),...
    OH_6_high_l_cum,OH_6_high_l_sort(:,2),...
    OH_6_low_h_cum,OH_6_low_h_sort(:,2),...
    OH_6_high_h_cum,OH_6_high_h_sort(:,2),...
    OH_7_low_l_cum,OH_7_low_l_sort(:,2),...
    OH_7_high_l_cum,OH_7_high_l_sort(:,2),...
    OH_7_low_h_cum,OH_7_low_h_sort(:,2),...
    OH_7_high_h_cum,OH_7_high_h_sort(:,2)];

OH_fval = zeros(1,size(OH_raw,2)/2);
OH_exp = OH_fval; OH_mid = OH_fval;
OH_fit = zeros(size(OH_raw));
OH_error = zeros(size(OH_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(OH_raw,2)/2;
if OH_raw(:,2*i-1) == 0;
else
    [OH_fval(i) OH_exp(i) OH_mid(i) OH_fit(:,2*i-1) OH_error(:,i)] = curve_fitting(OH_raw(:,2*i-1),OH_raw(:,2*i),x0,lb,ub);
    OH_fit(:,2*i) = OH_raw(:,2*i);
    if OH_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(OH_raw(:,2*i-1),OH_raw(:,2*i),x0,lb,ub);
        if fval < OH_fval(i);
            OH_fval(i) = fval; OH_exp(i) = exp; OH_mid(i) = mid;
            OH_fit(:,2*i-1) = fit;
        end
    end
end
end

OR_raw = [OR_1_low_l_cum,OR_1_low_l_sort(:,2),...
    OR_1_high_l_cum,OR_1_high_l_sort(:,2),...
    OR_1_low_h_cum,OR_1_low_h_sort(:,2),...
    OR_1_high_h_cum,OR_1_high_h_sort(:,2),...
    OR_2_low_l_cum,OR_2_low_l_sort(:,2),...
    OR_2_high_l_cum,OR_2_high_l_sort(:,2),...
    OR_2_low_h_cum,OR_2_low_h_sort(:,2),...
    OR_2_high_h_cum,OR_2_high_h_sort(:,2),...
    OR_3_low_l_cum,OR_3_low_l_sort(:,2),...
    OR_3_high_l_cum,OR_3_high_l_sort(:,2),...
    OR_3_low_h_cum,OR_3_low_h_sort(:,2),...
    OR_3_high_h_cum,OR_3_high_h_sort(:,2),...
    OR_4_low_l_cum,OR_4_low_l_sort(:,2),...
    OR_4_high_l_cum,OR_4_high_l_sort(:,2),...
    OR_4_low_h_cum,OR_4_low_h_sort(:,2),...
    OR_4_high_h_cum,OR_4_high_h_sort(:,2),...
    OR_5_low_l_cum,OR_5_low_l_sort(:,2),...
    OR_5_high_l_cum,OR_5_high_l_sort(:,2),...
    OR_5_low_h_cum,OR_5_low_h_sort(:,2),...
    OR_5_high_h_cum,OR_5_high_h_sort(:,2),...
    OR_6_low_l_cum,OR_6_low_l_sort(:,2),...
    OR_6_high_l_cum,OR_6_high_l_sort(:,2),...
    OR_6_low_h_cum,OR_6_low_h_sort(:,2),...
    OR_6_high_h_cum,OR_6_high_h_sort(:,2),...
    OR_7_low_l_cum,OR_7_low_l_sort(:,2),...
    OR_7_high_l_cum,OR_7_high_l_sort(:,2),...
    OR_7_low_h_cum,OR_7_low_h_sort(:,2),...
    OR_7_high_h_cum,OR_7_high_h_sort(:,2)];

OR_fval = zeros(1,size(OR_raw,2)/2);
OR_exp = OR_fval; OR_mid = OR_fval;
OR_fit = zeros(size(OR_raw));
OR_error = zeros(size(OR_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(OR_raw,2)/2;
if OR_raw(:,2*i-1) == 0;
else
    [OR_fval(i) OR_exp(i) OR_mid(i) OR_fit(:,2*i-1) OR_error(:,i)] = curve_fitting(OR_raw(:,2*i-1),OR_raw(:,2*i),x0,lb,ub);
    OR_fit(:,2*i) = OR_raw(:,2*i);
    if OR_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(OR_raw(:,2*i-1),OR_raw(:,2*i),x0,lb,ub);
        if fval < OR_fval(i);
            OR_fval(i) = fval; OR_exp(i) = exp; OR_mid(i) = mid;
            OR_fit(:,2*i-1) = fit;
        end
    end
end
end

PA_raw = [PA_1_low_l_cum,PA_1_low_l_sort(:,2),...
    PA_1_high_l_cum,PA_1_high_l_sort(:,2),...
    PA_1_low_h_cum,PA_1_low_h_sort(:,2),...
    PA_1_high_h_cum,PA_1_high_h_sort(:,2),...
    PA_2_low_l_cum,PA_2_low_l_sort(:,2),...
    PA_2_high_l_cum,PA_2_high_l_sort(:,2),...
    PA_2_low_h_cum,PA_2_low_h_sort(:,2),...
    PA_2_high_h_cum,PA_2_high_h_sort(:,2),...
    PA_3_low_l_cum,PA_3_low_l_sort(:,2),...
    PA_3_high_l_cum,PA_3_high_l_sort(:,2),...
    PA_3_low_h_cum,PA_3_low_h_sort(:,2),...
    PA_3_high_h_cum,PA_3_high_h_sort(:,2),...
    PA_4_low_l_cum,PA_4_low_l_sort(:,2),...
    PA_4_high_l_cum,PA_4_high_l_sort(:,2),...
    PA_4_low_h_cum,PA_4_low_h_sort(:,2),...
    PA_4_high_h_cum,PA_4_high_h_sort(:,2),...
    PA_5_low_l_cum,PA_5_low_l_sort(:,2),...
    PA_5_high_l_cum,PA_5_high_l_sort(:,2),...
    PA_5_low_h_cum,PA_5_low_h_sort(:,2),...
    PA_5_high_h_cum,PA_5_high_h_sort(:,2),...
    PA_6_low_l_cum,PA_6_low_l_sort(:,2),...
    PA_6_high_l_cum,PA_6_high_l_sort(:,2),...
    PA_6_low_h_cum,PA_6_low_h_sort(:,2),...
    PA_6_high_h_cum,PA_6_high_h_sort(:,2),...
    PA_7_low_l_cum,PA_7_low_l_sort(:,2),...
    PA_7_high_l_cum,PA_7_high_l_sort(:,2),...
    PA_7_low_h_cum,PA_7_low_h_sort(:,2),...
    PA_7_high_h_cum,PA_7_high_h_sort(:,2)];

PA_fval = zeros(1,size(PA_raw,2)/2);
PA_exp = PA_fval; PA_mid = PA_fval;
PA_fit = zeros(size(PA_raw));
PA_error = zeros(size(PA_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(PA_raw,2)/2;
if PA_raw(:,2*i-1) == 0;
else
    [PA_fval(i) PA_exp(i) PA_mid(i) PA_fit(:,2*i-1) PA_error(:,i)] = curve_fitting(PA_raw(:,2*i-1),PA_raw(:,2*i),x0,lb,ub);
    PA_fit(:,2*i) = PA_raw(:,2*i);
    if PA_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(PA_raw(:,2*i-1),PA_raw(:,2*i),x0,lb,ub);
        if fval < PA_fval(i);
            PA_fval(i) = fval; PA_exp(i) = exp; PA_mid(i) = mid;
            PA_fit(:,2*i-1) = fit;
        end
    end
end
end

RI_raw = [RI_1_low_l_cum,RI_1_low_l_sort(:,2),...
    RI_1_high_l_cum,RI_1_high_l_sort(:,2),...
    RI_1_low_h_cum,RI_1_low_h_sort(:,2),...
    RI_1_high_h_cum,RI_1_high_h_sort(:,2),...
    RI_2_low_l_cum,RI_2_low_l_sort(:,2),...
    RI_2_high_l_cum,RI_2_high_l_sort(:,2),...
    RI_2_low_h_cum,RI_2_low_h_sort(:,2),...
    RI_2_high_h_cum,RI_2_high_h_sort(:,2),...
    RI_3_low_l_cum,RI_3_low_l_sort(:,2),...
    RI_3_high_l_cum,RI_3_high_l_sort(:,2),...
    RI_3_low_h_cum,RI_3_low_h_sort(:,2),...
    RI_3_high_h_cum,RI_3_high_h_sort(:,2),...
    RI_4_low_l_cum,RI_4_low_l_sort(:,2),...
    RI_4_high_l_cum,RI_4_high_l_sort(:,2),...
    RI_4_low_h_cum,RI_4_low_h_sort(:,2),...
    RI_4_high_h_cum,RI_4_high_h_sort(:,2),...
    RI_5_low_l_cum,RI_5_low_l_sort(:,2),...
    RI_5_high_l_cum,RI_5_high_l_sort(:,2),...
    RI_5_low_h_cum,RI_5_low_h_sort(:,2),...
    RI_5_high_h_cum,RI_5_high_h_sort(:,2),...
    RI_6_low_l_cum,RI_6_low_l_sort(:,2),...
    RI_6_high_l_cum,RI_6_high_l_sort(:,2),...
    RI_6_low_h_cum,RI_6_low_h_sort(:,2),...
    RI_6_high_h_cum,RI_6_high_h_sort(:,2),...
    RI_7_low_l_cum,RI_7_low_l_sort(:,2),...
    RI_7_high_l_cum,RI_7_high_l_sort(:,2),...
    RI_7_low_h_cum,RI_7_low_h_sort(:,2),...
    RI_7_high_h_cum,RI_7_high_h_sort(:,2)];

RI_fval = zeros(1,size(RI_raw,2)/2);
RI_exp = RI_fval; RI_mid = RI_fval;
RI_fit = zeros(size(RI_raw));
RI_error = zeros(size(RI_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(RI_raw,2)/2;
if RI_raw(:,2*i-1) == 0;
else
    [RI_fval(i) RI_exp(i) RI_mid(i) RI_fit(:,2*i-1) RI_error(:,i)] = curve_fitting(RI_raw(:,2*i-1),RI_raw(:,2*i),x0,lb,ub);
    RI_fit(:,2*i) = RI_raw(:,2*i);
    if RI_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(RI_raw(:,2*i-1),RI_raw(:,2*i),x0,lb,ub);
        if fval < RI_fval(i);
            RI_fval(i) = fval; RI_exp(i) = exp; RI_mid(i) = mid;
            RI_fit(:,2*i-1) = fit;
        end
    end
end
end

SC_raw = [SC_1_low_l_cum,SC_1_low_l_sort(:,2),...
    SC_1_high_l_cum,SC_1_high_l_sort(:,2),...
    SC_1_low_h_cum,SC_1_low_h_sort(:,2),...
    SC_1_high_h_cum,SC_1_high_h_sort(:,2),...
    SC_2_low_l_cum,SC_2_low_l_sort(:,2),...
    SC_2_high_l_cum,SC_2_high_l_sort(:,2),...
    SC_2_low_h_cum,SC_2_low_h_sort(:,2),...
    SC_2_high_h_cum,SC_2_high_h_sort(:,2),...
    SC_3_low_l_cum,SC_3_low_l_sort(:,2),...
    SC_3_high_l_cum,SC_3_high_l_sort(:,2),...
    SC_3_low_h_cum,SC_3_low_h_sort(:,2),...
    SC_3_high_h_cum,SC_3_high_h_sort(:,2),...
    SC_4_low_l_cum,SC_4_low_l_sort(:,2),...
    SC_4_high_l_cum,SC_4_high_l_sort(:,2),...
    SC_4_low_h_cum,SC_4_low_h_sort(:,2),...
    SC_4_high_h_cum,SC_4_high_h_sort(:,2),...
    SC_5_low_l_cum,SC_5_low_l_sort(:,2),...
    SC_5_high_l_cum,SC_5_high_l_sort(:,2),...
    SC_5_low_h_cum,SC_5_low_h_sort(:,2),...
    SC_5_high_h_cum,SC_5_high_h_sort(:,2),...
    SC_6_low_l_cum,SC_6_low_l_sort(:,2),...
    SC_6_high_l_cum,SC_6_high_l_sort(:,2),...
    SC_6_low_h_cum,SC_6_low_h_sort(:,2),...
    SC_6_high_h_cum,SC_6_high_h_sort(:,2),...
    SC_7_low_l_cum,SC_7_low_l_sort(:,2),...
    SC_7_high_l_cum,SC_7_high_l_sort(:,2),...
    SC_7_low_h_cum,SC_7_low_h_sort(:,2),...
    SC_7_high_h_cum,SC_7_high_h_sort(:,2)];

SC_fval = zeros(1,size(SC_raw,2)/2);
SC_exp = SC_fval; SC_mid = SC_fval;
SC_fit = zeros(size(SC_raw));
SC_error = zeros(size(SC_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(SC_raw,2)/2;
if SC_raw(:,2*i-1) == 0;
else
    [SC_fval(i) SC_exp(i) SC_mid(i) SC_fit(:,2*i-1) SC_error(:,i)] = curve_fitting(SC_raw(:,2*i-1),SC_raw(:,2*i),x0,lb,ub);
    SC_fit(:,2*i) = SC_raw(:,2*i);
    if SC_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(SC_raw(:,2*i-1),SC_raw(:,2*i),x0,lb,ub);
        if fval < SC_fval(i);
            SC_fval(i) = fval; SC_exp(i) = exp; SC_mid(i) = mid;
            SC_fit(:,2*i-1) = fit;
        end
    end
end
end

TX_raw = [TX_1_low_l_cum,TX_1_low_l_sort(:,2),...
    TX_1_high_l_cum,TX_1_high_l_sort(:,2),...
    TX_1_low_h_cum,TX_1_low_h_sort(:,2),...
    TX_1_high_h_cum,TX_1_high_h_sort(:,2),...
    TX_2_low_l_cum,TX_2_low_l_sort(:,2),...
    TX_2_high_l_cum,TX_2_high_l_sort(:,2),...
    TX_2_low_h_cum,TX_2_low_h_sort(:,2),...
    TX_2_high_h_cum,TX_2_high_h_sort(:,2),...
    TX_3_low_l_cum,TX_3_low_l_sort(:,2),...
    TX_3_high_l_cum,TX_3_high_l_sort(:,2),...
    TX_3_low_h_cum,TX_3_low_h_sort(:,2),...
    TX_3_high_h_cum,TX_3_high_h_sort(:,2),...
    TX_4_low_l_cum,TX_4_low_l_sort(:,2),...
    TX_4_high_l_cum,TX_4_high_l_sort(:,2),...
    TX_4_low_h_cum,TX_4_low_h_sort(:,2),...
    TX_4_high_h_cum,TX_4_high_h_sort(:,2),...
    TX_5_low_l_cum,TX_5_low_l_sort(:,2),...
    TX_5_high_l_cum,TX_5_high_l_sort(:,2),...
    TX_5_low_h_cum,TX_5_low_h_sort(:,2),...
    TX_5_high_h_cum,TX_5_high_h_sort(:,2),...
    TX_6_low_l_cum,TX_6_low_l_sort(:,2),...
    TX_6_high_l_cum,TX_6_high_l_sort(:,2),...
    TX_6_low_h_cum,TX_6_low_h_sort(:,2),...
    TX_6_high_h_cum,TX_6_high_h_sort(:,2),...
    TX_7_low_l_cum,TX_7_low_l_sort(:,2),...
    TX_7_high_l_cum,TX_7_high_l_sort(:,2),...
    TX_7_low_h_cum,TX_7_low_h_sort(:,2),...
    TX_7_high_h_cum,TX_7_high_h_sort(:,2)];

TX_fval = zeros(1,size(TX_raw,2)/2);
TX_exp = TX_fval; TX_mid = TX_fval;
TX_fit = zeros(size(TX_raw));
TX_error = zeros(size(TX_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(TX_raw,2)/2;
if TX_raw(:,2*i-1) == 0;
else
    [TX_fval(i) TX_exp(i) TX_mid(i) TX_fit(:,2*i-1) TX_error(:,i)] = curve_fitting(TX_raw(:,2*i-1),TX_raw(:,2*i),x0,lb,ub);
    TX_fit(:,2*i) = TX_raw(:,2*i);
    if TX_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(TX_raw(:,2*i-1),TX_raw(:,2*i),x0,lb,ub);
        if fval < TX_fval(i);
            TX_fval(i) = fval; TX_exp(i) = exp; TX_mid(i) = mid;
            TX_fit(:,2*i-1) = fit;
        end
    end
end
end

VA_raw = [VA_1_low_l_cum,VA_1_low_l_sort(:,2),...
    VA_1_high_l_cum,VA_1_high_l_sort(:,2),...
    VA_1_low_h_cum,VA_1_low_h_sort(:,2),...
    VA_1_high_h_cum,VA_1_high_h_sort(:,2),...
    VA_2_low_l_cum,VA_2_low_l_sort(:,2),...
    VA_2_high_l_cum,VA_2_high_l_sort(:,2),...
    VA_2_low_h_cum,VA_2_low_h_sort(:,2),...
    VA_2_high_h_cum,VA_2_high_h_sort(:,2),...
    VA_3_low_l_cum,VA_3_low_l_sort(:,2),...
    VA_3_high_l_cum,VA_3_high_l_sort(:,2),...
    VA_3_low_h_cum,VA_3_low_h_sort(:,2),...
    VA_3_high_h_cum,VA_3_high_h_sort(:,2),...
    VA_4_low_l_cum,VA_4_low_l_sort(:,2),...
    VA_4_high_l_cum,VA_4_high_l_sort(:,2),...
    VA_4_low_h_cum,VA_4_low_h_sort(:,2),...
    VA_4_high_h_cum,VA_4_high_h_sort(:,2),...
    VA_5_low_l_cum,VA_5_low_l_sort(:,2),...
    VA_5_high_l_cum,VA_5_high_l_sort(:,2),...
    VA_5_low_h_cum,VA_5_low_h_sort(:,2),...
    VA_5_high_h_cum,VA_5_high_h_sort(:,2),...
    VA_6_low_l_cum,VA_6_low_l_sort(:,2),...
    VA_6_high_l_cum,VA_6_high_l_sort(:,2),...
    VA_6_low_h_cum,VA_6_low_h_sort(:,2),...
    VA_6_high_h_cum,VA_6_high_h_sort(:,2),...
    VA_7_low_l_cum,VA_7_low_l_sort(:,2),...
    VA_7_high_l_cum,VA_7_high_l_sort(:,2),...
    VA_7_low_h_cum,VA_7_low_h_sort(:,2),...
    VA_7_high_h_cum,VA_7_high_h_sort(:,2)];

VA_fval = zeros(1,size(VA_raw,2)/2);
VA_exp = VA_fval; VA_mid = VA_fval;
VA_fit = zeros(size(VA_raw));
VA_error = zeros(size(VA_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(VA_raw,2)/2;
if VA_raw(:,2*i-1) == 0;
else
    [VA_fval(i) VA_exp(i) VA_mid(i) VA_fit(:,2*i-1) VA_error(:,i)] = curve_fitting(VA_raw(:,2*i-1),VA_raw(:,2*i),x0,lb,ub);
    VA_fit(:,2*i) = VA_raw(:,2*i);
    if VA_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(VA_raw(:,2*i-1),VA_raw(:,2*i),x0,lb,ub);
        if fval < VA_fval(i);
            VA_fval(i) = fval; VA_exp(i) = exp; VA_mid(i) = mid;
            VA_fit(:,2*i-1) = fit;
        end
    end
end
end

WA_raw = [WA_1_low_l_cum,WA_1_low_l_sort(:,2),...
    WA_1_high_l_cum,WA_1_high_l_sort(:,2),...
    WA_1_low_h_cum,WA_1_low_h_sort(:,2),...
    WA_1_high_h_cum,WA_1_high_h_sort(:,2),...
    WA_2_low_l_cum,WA_2_low_l_sort(:,2),...
    WA_2_high_l_cum,WA_2_high_l_sort(:,2),...
    WA_2_low_h_cum,WA_2_low_h_sort(:,2),...
    WA_2_high_h_cum,WA_2_high_h_sort(:,2),...
    WA_3_low_l_cum,WA_3_low_l_sort(:,2),...
    WA_3_high_l_cum,WA_3_high_l_sort(:,2),...
    WA_3_low_h_cum,WA_3_low_h_sort(:,2),...
    WA_3_high_h_cum,WA_3_high_h_sort(:,2),...
    WA_4_low_l_cum,WA_4_low_l_sort(:,2),...
    WA_4_high_l_cum,WA_4_high_l_sort(:,2),...
    WA_4_low_h_cum,WA_4_low_h_sort(:,2),...
    WA_4_high_h_cum,WA_4_high_h_sort(:,2),...
    WA_5_low_l_cum,WA_5_low_l_sort(:,2),...
    WA_5_high_l_cum,WA_5_high_l_sort(:,2),...
    WA_5_low_h_cum,WA_5_low_h_sort(:,2),...
    WA_5_high_h_cum,WA_5_high_h_sort(:,2),...
    WA_6_low_l_cum,WA_6_low_l_sort(:,2),...
    WA_6_high_l_cum,WA_6_high_l_sort(:,2),...
    WA_6_low_h_cum,WA_6_low_h_sort(:,2),...
    WA_6_high_h_cum,WA_6_high_h_sort(:,2),...
    WA_7_low_l_cum,WA_7_low_l_sort(:,2),...
    WA_7_high_l_cum,WA_7_high_l_sort(:,2),...
    WA_7_low_h_cum,WA_7_low_h_sort(:,2),...
    WA_7_high_h_cum,WA_7_high_h_sort(:,2)];

WA_fval = zeros(1,size(WA_raw,2)/2);
WA_exp = WA_fval; WA_mid = WA_fval;
WA_fit = zeros(size(WA_raw));
WA_error = zeros(size(WA_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(WA_raw,2)/2;
if WA_raw(:,2*i-1) == 0;
else
    [WA_fval(i) WA_exp(i) WA_mid(i) WA_fit(:,2*i-1) WA_error(:,i)] = curve_fitting(WA_raw(:,2*i-1),WA_raw(:,2*i),x0,lb,ub);
    WA_fit(:,2*i) = WA_raw(:,2*i);
    if WA_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(WA_raw(:,2*i-1),WA_raw(:,2*i),x0,lb,ub);
        if fval < WA_fval(i);
            WA_fval(i) = fval; WA_exp(i) = exp; WA_mid(i) = mid;
            WA_fit(:,2*i-1) = fit;
        end
    end
end
end

WI_raw = [WI_1_low_l_cum,WI_1_low_l_sort(:,2),...
    WI_1_high_l_cum,WI_1_high_l_sort(:,2),...
    WI_1_low_h_cum,WI_1_low_h_sort(:,2),...
    WI_1_high_h_cum,WI_1_high_h_sort(:,2),...
    WI_2_low_l_cum,WI_2_low_l_sort(:,2),...
    WI_2_high_l_cum,WI_2_high_l_sort(:,2),...
    WI_2_low_h_cum,WI_2_low_h_sort(:,2),...
    WI_2_high_h_cum,WI_2_high_h_sort(:,2),...
    WI_3_low_l_cum,WI_3_low_l_sort(:,2),...
    WI_3_high_l_cum,WI_3_high_l_sort(:,2),...
    WI_3_low_h_cum,WI_3_low_h_sort(:,2),...
    WI_3_high_h_cum,WI_3_high_h_sort(:,2),...
    WI_4_low_l_cum,WI_4_low_l_sort(:,2),...
    WI_4_high_l_cum,WI_4_high_l_sort(:,2),...
    WI_4_low_h_cum,WI_4_low_h_sort(:,2),...
    WI_4_high_h_cum,WI_4_high_h_sort(:,2),...
    WI_5_low_l_cum,WI_5_low_l_sort(:,2),...
    WI_5_high_l_cum,WI_5_high_l_sort(:,2),...
    WI_5_low_h_cum,WI_5_low_h_sort(:,2),...
    WI_5_high_h_cum,WI_5_high_h_sort(:,2),...
    WI_6_low_l_cum,WI_6_low_l_sort(:,2),...
    WI_6_high_l_cum,WI_6_high_l_sort(:,2),...
    WI_6_low_h_cum,WI_6_low_h_sort(:,2),...
    WI_6_high_h_cum,WI_6_high_h_sort(:,2),...
    WI_7_low_l_cum,WI_7_low_l_sort(:,2),...
    WI_7_high_l_cum,WI_7_high_l_sort(:,2),...
    WI_7_low_h_cum,WI_7_low_h_sort(:,2),...
    WI_7_high_h_cum,WI_7_high_h_sort(:,2)];

WI_fval = zeros(1,size(WI_raw,2)/2);
WI_exp = WI_fval; WI_mid = WI_fval;
WI_fit = zeros(size(WI_raw));
WI_error = zeros(size(WI_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(WI_raw,2)/2;
if WI_raw(:,2*i-1) == 0;
else
    [WI_fval(i) WI_exp(i) WI_mid(i) WI_fit(:,2*i-1) WI_error(:,i)] = curve_fitting(WI_raw(:,2*i-1),WI_raw(:,2*i),x0,lb,ub);
    WI_fit(:,2*i) = WI_raw(:,2*i);
    if WI_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(WI_raw(:,2*i-1),WI_raw(:,2*i),x0,lb,ub);
        if fval < WI_fval(i);
            WI_fval(i) = fval; WI_exp(i) = exp; WI_mid(i) = mid;
            WI_fit(:,2*i-1) = fit;
        end
    end
end
end

Africa_Eastern_raw = [Africa_Eastern_c4_low_l_cum,Africa_Eastern_c4_low_l_sort(:,2),...
    Africa_Eastern_c4_high_l_cum,Africa_Eastern_c4_high_l_sort(:,2),...
    Africa_Eastern_c4_low_h_cum,Africa_Eastern_c4_low_h_sort(:,2),...
    Africa_Eastern_c4_high_h_cum,Africa_Eastern_c4_high_h_sort(:,2),...
    Africa_Eastern_c5_low_l_cum,Africa_Eastern_c5_low_l_sort(:,2),...
    Africa_Eastern_c5_high_l_cum,Africa_Eastern_c5_high_l_sort(:,2),...
    Africa_Eastern_c5_low_h_cum,Africa_Eastern_c5_low_h_sort(:,2),...
    Africa_Eastern_c5_high_h_cum,Africa_Eastern_c5_high_h_sort(:,2),...
    Africa_Eastern_c6_low_l_cum,Africa_Eastern_c6_low_l_sort(:,2),...
    Africa_Eastern_c6_high_l_cum,Africa_Eastern_c6_high_l_sort(:,2),...
    Africa_Eastern_c6_low_h_cum,Africa_Eastern_c6_low_h_sort(:,2),...
    Africa_Eastern_c6_high_h_cum,Africa_Eastern_c6_high_h_sort(:,2),...
    Africa_Eastern_c7_low_l_cum,Africa_Eastern_c7_low_l_sort(:,2),...
    Africa_Eastern_c7_high_l_cum,Africa_Eastern_c7_high_l_sort(:,2),...
    Africa_Eastern_c7_low_h_cum,Africa_Eastern_c7_low_h_sort(:,2),...
    Africa_Eastern_c7_high_h_cum,Africa_Eastern_c7_high_h_sort(:,2),...
    Africa_Eastern_c89_low_l_cum,Africa_Eastern_c89_low_l_sort(:,2),...
    Africa_Eastern_c89_high_l_cum,Africa_Eastern_c89_high_l_sort(:,2),...
    Africa_Eastern_c89_low_h_cum,Africa_Eastern_c89_low_h_sort(:,2),...
    Africa_Eastern_c89_high_h_cum,Africa_Eastern_c89_high_h_sort(:,2)];

Africa_Eastern_fval = zeros(1,size(Africa_Eastern_raw,2)/2);
Africa_Eastern_exp = Africa_Eastern_fval; Africa_Eastern_mid = Africa_Eastern_fval;
Africa_Eastern_fit = zeros(size(Africa_Eastern_raw));
Africa_Eastern_error = zeros(size(Africa_Eastern_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Africa_Eastern_raw,2)/2;
if Africa_Eastern_raw(:,2*i-1) == 0;
else
    [Africa_Eastern_fval(i) Africa_Eastern_exp(i) Africa_Eastern_mid(i) Africa_Eastern_fit(:,2*i-1) Africa_Eastern_error(:,i)] = curve_fitting(Africa_Eastern_raw(:,2*i-1),Africa_Eastern_raw(:,2*i),x0,lb,ub);
    Africa_Eastern_fit(:,2*i) = Africa_Eastern_raw(:,2*i);
    if Africa_Eastern_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Africa_Eastern_raw(:,2*i-1),Africa_Eastern_raw(:,2*i),x0,lb,ub);
        if fval < Africa_Eastern_fval(i);
            Africa_Eastern_fval(i) = fval; Africa_Eastern_exp(i) = exp; Africa_Eastern_mid(i) = mid;
            Africa_Eastern_fit(:,2*i-1) = fit;
        end
    end
end
end

Africa_Northern_raw = [Africa_Northern_c4_low_l_cum,Africa_Northern_c4_low_l_sort(:,2),...
    Africa_Northern_c4_high_l_cum,Africa_Northern_c4_high_l_sort(:,2),...
    Africa_Northern_c4_low_h_cum,Africa_Northern_c4_low_h_sort(:,2),...
    Africa_Northern_c4_high_h_cum,Africa_Northern_c4_high_h_sort(:,2),...
    Africa_Northern_c5_low_l_cum,Africa_Northern_c5_low_l_sort(:,2),...
    Africa_Northern_c5_high_l_cum,Africa_Northern_c5_high_l_sort(:,2),...
    Africa_Northern_c5_low_h_cum,Africa_Northern_c5_low_h_sort(:,2),...
    Africa_Northern_c5_high_h_cum,Africa_Northern_c5_high_h_sort(:,2),...
    Africa_Northern_c6_low_l_cum,Africa_Northern_c6_low_l_sort(:,2),...
    Africa_Northern_c6_high_l_cum,Africa_Northern_c6_high_l_sort(:,2),...
    Africa_Northern_c6_low_h_cum,Africa_Northern_c6_low_h_sort(:,2),...
    Africa_Northern_c6_high_h_cum,Africa_Northern_c6_high_h_sort(:,2),...
    Africa_Northern_c7_low_l_cum,Africa_Northern_c7_low_l_sort(:,2),...
    Africa_Northern_c7_high_l_cum,Africa_Northern_c7_high_l_sort(:,2),...
    Africa_Northern_c7_low_h_cum,Africa_Northern_c7_low_h_sort(:,2),...
    Africa_Northern_c7_high_h_cum,Africa_Northern_c7_high_h_sort(:,2),...
    Africa_Northern_c89_low_l_cum,Africa_Northern_c89_low_l_sort(:,2),...
    Africa_Northern_c89_high_l_cum,Africa_Northern_c89_high_l_sort(:,2),...
    Africa_Northern_c89_low_h_cum,Africa_Northern_c89_low_h_sort(:,2),...
    Africa_Northern_c89_high_h_cum,Africa_Northern_c89_high_h_sort(:,2)];

Africa_Northern_fval = zeros(1,size(Africa_Northern_raw,2)/2);
Africa_Northern_exp = Africa_Northern_fval; Africa_Northern_mid = Africa_Northern_fval;
Africa_Northern_fit = zeros(size(Africa_Northern_raw));
Africa_Northern_error = zeros(size(Africa_Northern_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Africa_Northern_raw,2)/2;
if Africa_Northern_raw(:,2*i-1) == 0;
else
    [Africa_Northern_fval(i) Africa_Northern_exp(i) Africa_Northern_mid(i) Africa_Northern_fit(:,2*i-1) Africa_Northern_error(:,i)] = curve_fitting(Africa_Northern_raw(:,2*i-1),Africa_Northern_raw(:,2*i),x0,lb,ub);
    Africa_Northern_fit(:,2*i) = Africa_Northern_raw(:,2*i);
    if Africa_Northern_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Africa_Northern_raw(:,2*i-1),Africa_Northern_raw(:,2*i),x0,lb,ub);
        if fval < Africa_Northern_fval(i);
            Africa_Northern_fval(i) = fval; Africa_Northern_exp(i) = exp; Africa_Northern_mid(i) = mid;
            Africa_Northern_fit(:,2*i-1) = fit;
        end
    end
end
end

Africa_Southern_raw = [Africa_Southern_c4_low_l_cum,Africa_Southern_c4_low_l_sort(:,2),...
    Africa_Southern_c4_high_l_cum,Africa_Southern_c4_high_l_sort(:,2),...
    Africa_Southern_c4_low_h_cum,Africa_Southern_c4_low_h_sort(:,2),...
    Africa_Southern_c4_high_h_cum,Africa_Southern_c4_high_h_sort(:,2),...
    Africa_Southern_c5_low_l_cum,Africa_Southern_c5_low_l_sort(:,2),...
    Africa_Southern_c5_high_l_cum,Africa_Southern_c5_high_l_sort(:,2),...
    Africa_Southern_c5_low_h_cum,Africa_Southern_c5_low_h_sort(:,2),...
    Africa_Southern_c5_high_h_cum,Africa_Southern_c5_high_h_sort(:,2),...
    Africa_Southern_c6_low_l_cum,Africa_Southern_c6_low_l_sort(:,2),...
    Africa_Southern_c6_high_l_cum,Africa_Southern_c6_high_l_sort(:,2),...
    Africa_Southern_c6_low_h_cum,Africa_Southern_c6_low_h_sort(:,2),...
    Africa_Southern_c6_high_h_cum,Africa_Southern_c6_high_h_sort(:,2),...
    Africa_Southern_c7_low_l_cum,Africa_Southern_c7_low_l_sort(:,2),...
    Africa_Southern_c7_high_l_cum,Africa_Southern_c7_high_l_sort(:,2),...
    Africa_Southern_c7_low_h_cum,Africa_Southern_c7_low_h_sort(:,2),...
    Africa_Southern_c7_high_h_cum,Africa_Southern_c7_high_h_sort(:,2),...
    Africa_Southern_c89_low_l_cum,Africa_Southern_c89_low_l_sort(:,2),...
    Africa_Southern_c89_high_l_cum,Africa_Southern_c89_high_l_sort(:,2),...
    Africa_Southern_c89_low_h_cum,Africa_Southern_c89_low_h_sort(:,2),...
    Africa_Southern_c89_high_h_cum,Africa_Southern_c89_high_h_sort(:,2)];

Africa_Southern_fval = zeros(1,size(Africa_Southern_raw,2)/2);
Africa_Southern_exp = Africa_Southern_fval; Africa_Southern_mid = Africa_Southern_fval;
Africa_Southern_fit = zeros(size(Africa_Southern_raw));
Africa_Southern_error = zeros(size(Africa_Southern_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Africa_Southern_raw,2)/2;
if Africa_Southern_raw(:,2*i-1) == 0;
else
    [Africa_Southern_fval(i) Africa_Southern_exp(i) Africa_Southern_mid(i) Africa_Southern_fit(:,2*i-1) Africa_Southern_error(:,i)] = curve_fitting(Africa_Southern_raw(:,2*i-1),Africa_Southern_raw(:,2*i),x0,lb,ub);
    Africa_Southern_fit(:,2*i) = Africa_Southern_raw(:,2*i);
    if Africa_Southern_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Africa_Southern_raw(:,2*i-1),Africa_Southern_raw(:,2*i),x0,lb,ub);
        if fval < Africa_Southern_fval(i);
            Africa_Southern_fval(i) = fval; Africa_Southern_exp(i) = exp; Africa_Southern_mid(i) = mid;
            Africa_Southern_fit(:,2*i-1) = fit;
        end
    end
end
end

Africa_Western_raw = [Africa_Western_c4_low_l_cum,Africa_Western_c4_low_l_sort(:,2),...
    Africa_Western_c4_high_l_cum,Africa_Western_c4_high_l_sort(:,2),...
    Africa_Western_c4_low_h_cum,Africa_Western_c4_low_h_sort(:,2),...
    Africa_Western_c4_high_h_cum,Africa_Western_c4_high_h_sort(:,2),...
    Africa_Western_c5_low_l_cum,Africa_Western_c5_low_l_sort(:,2),...
    Africa_Western_c5_high_l_cum,Africa_Western_c5_high_l_sort(:,2),...
    Africa_Western_c5_low_h_cum,Africa_Western_c5_low_h_sort(:,2),...
    Africa_Western_c5_high_h_cum,Africa_Western_c5_high_h_sort(:,2),...
    Africa_Western_c6_low_l_cum,Africa_Western_c6_low_l_sort(:,2),...
    Africa_Western_c6_high_l_cum,Africa_Western_c6_high_l_sort(:,2),...
    Africa_Western_c6_low_h_cum,Africa_Western_c6_low_h_sort(:,2),...
    Africa_Western_c6_high_h_cum,Africa_Western_c6_high_h_sort(:,2),...
    Africa_Western_c7_low_l_cum,Africa_Western_c7_low_l_sort(:,2),...
    Africa_Western_c7_high_l_cum,Africa_Western_c7_high_l_sort(:,2),...
    Africa_Western_c7_low_h_cum,Africa_Western_c7_low_h_sort(:,2),...
    Africa_Western_c7_high_h_cum,Africa_Western_c7_high_h_sort(:,2),...
    Africa_Western_c89_low_l_cum,Africa_Western_c89_low_l_sort(:,2),...
    Africa_Western_c89_high_l_cum,Africa_Western_c89_high_l_sort(:,2),...
    Africa_Western_c89_low_h_cum,Africa_Western_c89_low_h_sort(:,2),...
    Africa_Western_c89_high_h_cum,Africa_Western_c89_high_h_sort(:,2)];

Africa_Western_fval = zeros(1,size(Africa_Western_raw,2)/2);
Africa_Western_exp = Africa_Western_fval; Africa_Western_mid = Africa_Western_fval;
Africa_Western_fit = zeros(size(Africa_Western_raw));
Africa_Western_error = zeros(size(Africa_Western_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Africa_Western_raw,2)/2;
if Africa_Western_raw(:,2*i-1) == 0;
else
    [Africa_Western_fval(i) Africa_Western_exp(i) Africa_Western_mid(i) Africa_Western_fit(:,2*i-1) Africa_Western_error(:,i)] = curve_fitting(Africa_Western_raw(:,2*i-1),Africa_Western_raw(:,2*i),x0,lb,ub);
    Africa_Western_fit(:,2*i) = Africa_Western_raw(:,2*i);
    if Africa_Western_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Africa_Western_raw(:,2*i-1),Africa_Western_raw(:,2*i),x0,lb,ub);
        if fval < Africa_Western_fval(i);
            Africa_Western_fval(i) = fval; Africa_Western_exp(i) = exp; Africa_Western_mid(i) = mid;
            Africa_Western_fit(:,2*i-1) = fit;
        end
    end
end
end

Australia_NZ_raw = [Australia_NZ_c4_low_l_cum,Australia_NZ_c4_low_l_sort(:,2),...
    Australia_NZ_c4_high_l_cum,Australia_NZ_c4_high_l_sort(:,2),...
    Australia_NZ_c4_low_h_cum,Australia_NZ_c4_low_h_sort(:,2),...
    Australia_NZ_c4_high_h_cum,Australia_NZ_c4_high_h_sort(:,2),...
    Australia_NZ_c5_low_l_cum,Australia_NZ_c5_low_l_sort(:,2),...
    Australia_NZ_c5_high_l_cum,Australia_NZ_c5_high_l_sort(:,2),...
    Australia_NZ_c5_low_h_cum,Australia_NZ_c5_low_h_sort(:,2),...
    Australia_NZ_c5_high_h_cum,Australia_NZ_c5_high_h_sort(:,2),...
    Australia_NZ_c6_low_l_cum,Australia_NZ_c6_low_l_sort(:,2),...
    Australia_NZ_c6_high_l_cum,Australia_NZ_c6_high_l_sort(:,2),...
    Australia_NZ_c6_low_h_cum,Australia_NZ_c6_low_h_sort(:,2),...
    Australia_NZ_c6_high_h_cum,Australia_NZ_c6_high_h_sort(:,2),...
    Australia_NZ_c7_low_l_cum,Australia_NZ_c7_low_l_sort(:,2),...
    Australia_NZ_c7_high_l_cum,Australia_NZ_c7_high_l_sort(:,2),...
    Australia_NZ_c7_low_h_cum,Australia_NZ_c7_low_h_sort(:,2),...
    Australia_NZ_c7_high_h_cum,Australia_NZ_c7_high_h_sort(:,2),...
    Australia_NZ_c89_low_l_cum,Australia_NZ_c89_low_l_sort(:,2),...
    Australia_NZ_c89_high_l_cum,Australia_NZ_c89_high_l_sort(:,2),...
    Australia_NZ_c89_low_h_cum,Australia_NZ_c89_low_h_sort(:,2),...
    Australia_NZ_c89_high_h_cum,Australia_NZ_c89_high_h_sort(:,2)];

Australia_NZ_fval = zeros(1,size(Australia_NZ_raw,2)/2);
Australia_NZ_exp = Australia_NZ_fval; Australia_NZ_mid = Australia_NZ_fval;
Australia_NZ_fit = zeros(size(Australia_NZ_raw));
Australia_NZ_error = zeros(size(Australia_NZ_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Australia_NZ_raw,2)/2;
if Australia_NZ_raw(:,2*i-1) == 0;
else
    [Australia_NZ_fval(i) Australia_NZ_exp(i) Australia_NZ_mid(i) Australia_NZ_fit(:,2*i-1) Australia_NZ_error(:,i)] = curve_fitting(Australia_NZ_raw(:,2*i-1),Australia_NZ_raw(:,2*i),x0,lb,ub);
    Australia_NZ_fit(:,2*i) = Australia_NZ_raw(:,2*i);
    if Australia_NZ_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Australia_NZ_raw(:,2*i-1),Australia_NZ_raw(:,2*i),x0,lb,ub);
        if fval < Australia_NZ_fval(i);
            Australia_NZ_fval(i) = fval; Australia_NZ_exp(i) = exp; Australia_NZ_mid(i) = mid;
            Australia_NZ_fit(:,2*i-1) = fit;
        end
    end
end
end

Brazil_raw = [Brazil_c4_low_l_cum,Brazil_c4_low_l_sort(:,2),...
    Brazil_c4_high_l_cum,Brazil_c4_high_l_sort(:,2),...
    Brazil_c4_low_h_cum,Brazil_c4_low_h_sort(:,2),...
    Brazil_c4_high_h_cum,Brazil_c4_high_h_sort(:,2),...
    Brazil_c5_low_l_cum,Brazil_c5_low_l_sort(:,2),...
    Brazil_c5_high_l_cum,Brazil_c5_high_l_sort(:,2),...
    Brazil_c5_low_h_cum,Brazil_c5_low_h_sort(:,2),...
    Brazil_c5_high_h_cum,Brazil_c5_high_h_sort(:,2),...
    Brazil_c6_low_l_cum,Brazil_c6_low_l_sort(:,2),...
    Brazil_c6_high_l_cum,Brazil_c6_high_l_sort(:,2),...
    Brazil_c6_low_h_cum,Brazil_c6_low_h_sort(:,2),...
    Brazil_c6_high_h_cum,Brazil_c6_high_h_sort(:,2),...
    Brazil_c7_low_l_cum,Brazil_c7_low_l_sort(:,2),...
    Brazil_c7_high_l_cum,Brazil_c7_high_l_sort(:,2),...
    Brazil_c7_low_h_cum,Brazil_c7_low_h_sort(:,2),...
    Brazil_c7_high_h_cum,Brazil_c7_high_h_sort(:,2),...
    Brazil_c89_low_l_cum,Brazil_c89_low_l_sort(:,2),...
    Brazil_c89_high_l_cum,Brazil_c89_high_l_sort(:,2),...
    Brazil_c89_low_h_cum,Brazil_c89_low_h_sort(:,2),...
    Brazil_c89_high_h_cum,Brazil_c89_high_h_sort(:,2)];

Brazil_fval = zeros(1,size(Brazil_raw,2)/2);
Brazil_exp = Brazil_fval; Brazil_mid = Brazil_fval;
Brazil_fit = zeros(size(Brazil_raw));
Brazil_error = zeros(size(Brazil_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Brazil_raw,2)/2;
if Brazil_raw(:,2*i-1) == 0;
else
    [Brazil_fval(i) Brazil_exp(i) Brazil_mid(i) Brazil_fit(:,2*i-1) Brazil_error(:,i)] = curve_fitting(Brazil_raw(:,2*i-1),Brazil_raw(:,2*i),x0,lb,ub);
    Brazil_fit(:,2*i) = Brazil_raw(:,2*i);
    if Brazil_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Brazil_raw(:,2*i-1),Brazil_raw(:,2*i),x0,lb,ub);
        if fval < Brazil_fval(i);
            Brazil_fval(i) = fval; Brazil_exp(i) = exp; Brazil_mid(i) = mid;
            Brazil_fit(:,2*i-1) = fit;
        end
    end
end
end

Canada_raw = [Canada_c4_low_l_cum,Canada_c4_low_l_sort(:,2),...
    Canada_c4_high_l_cum,Canada_c4_high_l_sort(:,2),...
    Canada_c4_low_h_cum,Canada_c4_low_h_sort(:,2),...
    Canada_c4_high_h_cum,Canada_c4_high_h_sort(:,2),...
    Canada_c5_low_l_cum,Canada_c5_low_l_sort(:,2),...
    Canada_c5_high_l_cum,Canada_c5_high_l_sort(:,2),...
    Canada_c5_low_h_cum,Canada_c5_low_h_sort(:,2),...
    Canada_c5_high_h_cum,Canada_c5_high_h_sort(:,2),...
    Canada_c6_low_l_cum,Canada_c6_low_l_sort(:,2),...
    Canada_c6_high_l_cum,Canada_c6_high_l_sort(:,2),...
    Canada_c6_low_h_cum,Canada_c6_low_h_sort(:,2),...
    Canada_c6_high_h_cum,Canada_c6_high_h_sort(:,2),...
    Canada_c7_low_l_cum,Canada_c7_low_l_sort(:,2),...
    Canada_c7_high_l_cum,Canada_c7_high_l_sort(:,2),...
    Canada_c7_low_h_cum,Canada_c7_low_h_sort(:,2),...
    Canada_c7_high_h_cum,Canada_c7_high_h_sort(:,2),...
    Canada_c89_low_l_cum,Canada_c89_low_l_sort(:,2),...
    Canada_c89_high_l_cum,Canada_c89_high_l_sort(:,2),...
    Canada_c89_low_h_cum,Canada_c89_low_h_sort(:,2),...
    Canada_c89_high_h_cum,Canada_c89_high_h_sort(:,2)];

Canada_fval = zeros(1,size(Canada_raw,2)/2);
Canada_exp = Canada_fval; Canada_mid = Canada_fval;
Canada_fit = zeros(size(Canada_raw));
Canada_error = zeros(size(Canada_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Canada_raw,2)/2;
if Canada_raw(:,2*i-1) == 0;
else
    [Canada_fval(i) Canada_exp(i) Canada_mid(i) Canada_fit(:,2*i-1) Canada_error(:,i)] = curve_fitting(Canada_raw(:,2*i-1),Canada_raw(:,2*i),x0,lb,ub);
    Canada_fit(:,2*i) = Canada_raw(:,2*i);
    if Canada_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Canada_raw(:,2*i-1),Canada_raw(:,2*i),x0,lb,ub);
        if fval < Canada_fval(i);
            Canada_fval(i) = fval; Canada_exp(i) = exp; Canada_mid(i) = mid;
            Canada_fit(:,2*i-1) = fit;
        end
    end
end
end

Central_America_and_Caribbean_raw = [Central_America_and_Caribbean_c4_low_l_cum,Central_America_and_Caribbean_c4_low_l_sort(:,2),...
    Central_America_and_Caribbean_c4_high_l_cum,Central_America_and_Caribbean_c4_high_l_sort(:,2),...
    Central_America_and_Caribbean_c4_low_h_cum,Central_America_and_Caribbean_c4_low_h_sort(:,2),...
    Central_America_and_Caribbean_c4_high_h_cum,Central_America_and_Caribbean_c4_high_h_sort(:,2),...
    Central_America_and_Caribbean_c5_low_l_cum,Central_America_and_Caribbean_c5_low_l_sort(:,2),...
    Central_America_and_Caribbean_c5_high_l_cum,Central_America_and_Caribbean_c5_high_l_sort(:,2),...
    Central_America_and_Caribbean_c5_low_h_cum,Central_America_and_Caribbean_c5_low_h_sort(:,2),...
    Central_America_and_Caribbean_c5_high_h_cum,Central_America_and_Caribbean_c5_high_h_sort(:,2),...
    Central_America_and_Caribbean_c6_low_l_cum,Central_America_and_Caribbean_c6_low_l_sort(:,2),...
    Central_America_and_Caribbean_c6_high_l_cum,Central_America_and_Caribbean_c6_high_l_sort(:,2),...
    Central_America_and_Caribbean_c6_low_h_cum,Central_America_and_Caribbean_c6_low_h_sort(:,2),...
    Central_America_and_Caribbean_c6_high_h_cum,Central_America_and_Caribbean_c6_high_h_sort(:,2),...
    Central_America_and_Caribbean_c7_low_l_cum,Central_America_and_Caribbean_c7_low_l_sort(:,2),...
    Central_America_and_Caribbean_c7_high_l_cum,Central_America_and_Caribbean_c7_high_l_sort(:,2),...
    Central_America_and_Caribbean_c7_low_h_cum,Central_America_and_Caribbean_c7_low_h_sort(:,2),...
    Central_America_and_Caribbean_c7_high_h_cum,Central_America_and_Caribbean_c7_high_h_sort(:,2),...
    Central_America_and_Caribbean_c89_low_l_cum,Central_America_and_Caribbean_c89_low_l_sort(:,2),...
    Central_America_and_Caribbean_c89_high_l_cum,Central_America_and_Caribbean_c89_high_l_sort(:,2),...
    Central_America_and_Caribbean_c89_low_h_cum,Central_America_and_Caribbean_c89_low_h_sort(:,2),...
    Central_America_and_Caribbean_c89_high_h_cum,Central_America_and_Caribbean_c89_high_h_sort(:,2)];

Central_America_and_Caribbean_fval = zeros(1,size(Central_America_and_Caribbean_raw,2)/2);
Central_America_and_Caribbean_exp = Central_America_and_Caribbean_fval; Central_America_and_Caribbean_mid = Central_America_and_Caribbean_fval;
Central_America_and_Caribbean_fit = zeros(size(Central_America_and_Caribbean_raw));
Central_America_and_Caribbean_error = zeros(size(Central_America_and_Caribbean_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Central_America_and_Caribbean_raw,2)/2;
if Central_America_and_Caribbean_raw(:,2*i-1) == 0;
else
    [Central_America_and_Caribbean_fval(i) Central_America_and_Caribbean_exp(i) Central_America_and_Caribbean_mid(i) Central_America_and_Caribbean_fit(:,2*i-1) Central_America_and_Caribbean_error(:,i)] = curve_fitting(Central_America_and_Caribbean_raw(:,2*i-1),Central_America_and_Caribbean_raw(:,2*i),x0,lb,ub);
    Central_America_and_Caribbean_fit(:,2*i) = Central_America_and_Caribbean_raw(:,2*i);
    if Central_America_and_Caribbean_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Central_America_and_Caribbean_raw(:,2*i-1),Central_America_and_Caribbean_raw(:,2*i),x0,lb,ub);
        if fval < Central_America_and_Caribbean_fval(i);
            Central_America_and_Caribbean_fval(i) = fval; Central_America_and_Caribbean_exp(i) = exp; Central_America_and_Caribbean_mid(i) = mid;
            Central_America_and_Caribbean_fit(:,2*i-1) = fit;
        end
    end
end
end

Central_Asia_raw = [Central_Asia_c4_low_l_cum,Central_Asia_c4_low_l_sort(:,2),...
    Central_Asia_c4_high_l_cum,Central_Asia_c4_high_l_sort(:,2),...
    Central_Asia_c4_low_h_cum,Central_Asia_c4_low_h_sort(:,2),...
    Central_Asia_c4_high_h_cum,Central_Asia_c4_high_h_sort(:,2),...
    Central_Asia_c5_low_l_cum,Central_Asia_c5_low_l_sort(:,2),...
    Central_Asia_c5_high_l_cum,Central_Asia_c5_high_l_sort(:,2),...
    Central_Asia_c5_low_h_cum,Central_Asia_c5_low_h_sort(:,2),...
    Central_Asia_c5_high_h_cum,Central_Asia_c5_high_h_sort(:,2),...
    Central_Asia_c6_low_l_cum,Central_Asia_c6_low_l_sort(:,2),...
    Central_Asia_c6_high_l_cum,Central_Asia_c6_high_l_sort(:,2),...
    Central_Asia_c6_low_h_cum,Central_Asia_c6_low_h_sort(:,2),...
    Central_Asia_c6_high_h_cum,Central_Asia_c6_high_h_sort(:,2),...
    Central_Asia_c7_low_l_cum,Central_Asia_c7_low_l_sort(:,2),...
    Central_Asia_c7_high_l_cum,Central_Asia_c7_high_l_sort(:,2),...
    Central_Asia_c7_low_h_cum,Central_Asia_c7_low_h_sort(:,2),...
    Central_Asia_c7_high_h_cum,Central_Asia_c7_high_h_sort(:,2),...
    Central_Asia_c89_low_l_cum,Central_Asia_c89_low_l_sort(:,2),...
    Central_Asia_c89_high_l_cum,Central_Asia_c89_high_l_sort(:,2),...
    Central_Asia_c89_low_h_cum,Central_Asia_c89_low_h_sort(:,2),...
    Central_Asia_c89_high_h_cum,Central_Asia_c89_high_h_sort(:,2)];

Central_Asia_fval = zeros(1,size(Central_Asia_raw,2)/2);
Central_Asia_exp = Central_Asia_fval; Central_Asia_mid = Central_Asia_fval;
Central_Asia_fit = zeros(size(Central_Asia_raw));
Central_Asia_error = zeros(size(Central_Asia_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Central_Asia_raw,2)/2;
if Central_Asia_raw(:,2*i-1) == 0;
else
    [Central_Asia_fval(i) Central_Asia_exp(i) Central_Asia_mid(i) Central_Asia_fit(:,2*i-1) Central_Asia_error(:,i)] = curve_fitting(Central_Asia_raw(:,2*i-1),Central_Asia_raw(:,2*i),x0,lb,ub);
    Central_Asia_fit(:,2*i) = Central_Asia_raw(:,2*i);
    if Central_Asia_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Central_Asia_raw(:,2*i-1),Central_Asia_raw(:,2*i),x0,lb,ub);
        if fval < Central_Asia_fval(i);
            Central_Asia_fval(i) = fval; Central_Asia_exp(i) = exp; Central_Asia_mid(i) = mid;
            Central_Asia_fit(:,2*i-1) = fit;
        end
    end
end
end

China_raw = [China_c4_low_l_cum,China_c4_low_l_sort(:,2),...
    China_c4_high_l_cum,China_c4_high_l_sort(:,2),...
    China_c4_low_h_cum,China_c4_low_h_sort(:,2),...
    China_c4_high_h_cum,China_c4_high_h_sort(:,2),...
    China_c5_low_l_cum,China_c5_low_l_sort(:,2),...
    China_c5_high_l_cum,China_c5_high_l_sort(:,2),...
    China_c5_low_h_cum,China_c5_low_h_sort(:,2),...
    China_c5_high_h_cum,China_c5_high_h_sort(:,2),...
    China_c6_low_l_cum,China_c6_low_l_sort(:,2),...
    China_c6_high_l_cum,China_c6_high_l_sort(:,2),...
    China_c6_low_h_cum,China_c6_low_h_sort(:,2),...
    China_c6_high_h_cum,China_c6_high_h_sort(:,2),...
    China_c7_low_l_cum,China_c7_low_l_sort(:,2),...
    China_c7_high_l_cum,China_c7_high_l_sort(:,2),...
    China_c7_low_h_cum,China_c7_low_h_sort(:,2),...
    China_c7_high_h_cum,China_c7_high_h_sort(:,2),...
    China_c89_low_l_cum,China_c89_low_l_sort(:,2),...
    China_c89_high_l_cum,China_c89_high_l_sort(:,2),...
    China_c89_low_h_cum,China_c89_low_h_sort(:,2),...
    China_c89_high_h_cum,China_c89_high_h_sort(:,2)];

China_fval = zeros(1,size(China_raw,2)/2);
China_exp = China_fval; China_mid = China_fval;
China_fit = zeros(size(China_raw));
China_error = zeros(size(China_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(China_raw,2)/2;
if China_raw(:,2*i-1) == 0;
else
    [China_fval(i) China_exp(i) China_mid(i) China_fit(:,2*i-1) China_error(:,i)] = curve_fitting(China_raw(:,2*i-1),China_raw(:,2*i),x0,lb,ub);
    China_fit(:,2*i) = China_raw(:,2*i);
    if China_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(China_raw(:,2*i-1),China_raw(:,2*i),x0,lb,ub);
        if fval < China_fval(i);
            China_fval(i) = fval; China_exp(i) = exp; China_mid(i) = mid;
            China_fit(:,2*i-1) = fit;
        end
    end
end
end

EU_12_raw = [EU_12_c4_low_l_cum,EU_12_c4_low_l_sort(:,2),...
    EU_12_c4_high_l_cum,EU_12_c4_high_l_sort(:,2),...
    EU_12_c4_low_h_cum,EU_12_c4_low_h_sort(:,2),...
    EU_12_c4_high_h_cum,EU_12_c4_high_h_sort(:,2),...
    EU_12_c5_low_l_cum,EU_12_c5_low_l_sort(:,2),...
    EU_12_c5_high_l_cum,EU_12_c5_high_l_sort(:,2),...
    EU_12_c5_low_h_cum,EU_12_c5_low_h_sort(:,2),...
    EU_12_c5_high_h_cum,EU_12_c5_high_h_sort(:,2),...
    EU_12_c6_low_l_cum,EU_12_c6_low_l_sort(:,2),...
    EU_12_c6_high_l_cum,EU_12_c6_high_l_sort(:,2),...
    EU_12_c6_low_h_cum,EU_12_c6_low_h_sort(:,2),...
    EU_12_c6_high_h_cum,EU_12_c6_high_h_sort(:,2),...
    EU_12_c7_low_l_cum,EU_12_c7_low_l_sort(:,2),...
    EU_12_c7_high_l_cum,EU_12_c7_high_l_sort(:,2),...
    EU_12_c7_low_h_cum,EU_12_c7_low_h_sort(:,2),...
    EU_12_c7_high_h_cum,EU_12_c7_high_h_sort(:,2),...
    EU_12_c89_low_l_cum,EU_12_c89_low_l_sort(:,2),...
    EU_12_c89_high_l_cum,EU_12_c89_high_l_sort(:,2),...
    EU_12_c89_low_h_cum,EU_12_c89_low_h_sort(:,2),...
    EU_12_c89_high_h_cum,EU_12_c89_high_h_sort(:,2)];

EU_12_fval = zeros(1,size(EU_12_raw,2)/2);
EU_12_exp = EU_12_fval; EU_12_mid = EU_12_fval;
EU_12_fit = zeros(size(EU_12_raw));
EU_12_error = zeros(size(EU_12_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(EU_12_raw,2)/2;
if EU_12_raw(:,2*i-1) == 0;
else
    [EU_12_fval(i) EU_12_exp(i) EU_12_mid(i) EU_12_fit(:,2*i-1) EU_12_error(:,i)] = curve_fitting(EU_12_raw(:,2*i-1),EU_12_raw(:,2*i),x0,lb,ub);
    EU_12_fit(:,2*i) = EU_12_raw(:,2*i);
    if EU_12_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(EU_12_raw(:,2*i-1),EU_12_raw(:,2*i),x0,lb,ub);
        if fval < EU_12_fval(i);
            EU_12_fval(i) = fval; EU_12_exp(i) = exp; EU_12_mid(i) = mid;
            EU_12_fit(:,2*i-1) = fit;
        end
    end
end
end

EU_15_raw = [EU_15_c4_low_l_cum,EU_15_c4_low_l_sort(:,2),...
    EU_15_c4_high_l_cum,EU_15_c4_high_l_sort(:,2),...
    EU_15_c4_low_h_cum,EU_15_c4_low_h_sort(:,2),...
    EU_15_c4_high_h_cum,EU_15_c4_high_h_sort(:,2),...
    EU_15_c5_low_l_cum,EU_15_c5_low_l_sort(:,2),...
    EU_15_c5_high_l_cum,EU_15_c5_high_l_sort(:,2),...
    EU_15_c5_low_h_cum,EU_15_c5_low_h_sort(:,2),...
    EU_15_c5_high_h_cum,EU_15_c5_high_h_sort(:,2),...
    EU_15_c6_low_l_cum,EU_15_c6_low_l_sort(:,2),...
    EU_15_c6_high_l_cum,EU_15_c6_high_l_sort(:,2),...
    EU_15_c6_low_h_cum,EU_15_c6_low_h_sort(:,2),...
    EU_15_c6_high_h_cum,EU_15_c6_high_h_sort(:,2),...
    EU_15_c7_low_l_cum,EU_15_c7_low_l_sort(:,2),...
    EU_15_c7_high_l_cum,EU_15_c7_high_l_sort(:,2),...
    EU_15_c7_low_h_cum,EU_15_c7_low_h_sort(:,2),...
    EU_15_c7_high_h_cum,EU_15_c7_high_h_sort(:,2),...
    EU_15_c89_low_l_cum,EU_15_c89_low_l_sort(:,2),...
    EU_15_c89_high_l_cum,EU_15_c89_high_l_sort(:,2),...
    EU_15_c89_low_h_cum,EU_15_c89_low_h_sort(:,2),...
    EU_15_c89_high_h_cum,EU_15_c89_high_h_sort(:,2)];

EU_15_fval = zeros(1,size(EU_15_raw,2)/2);
EU_15_exp = EU_15_fval; EU_15_mid = EU_15_fval;
EU_15_fit = zeros(size(EU_15_raw));
EU_15_error = zeros(size(EU_15_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(EU_15_raw,2)/2;
if EU_15_raw(:,2*i-1) == 0;
else
    [EU_15_fval(i) EU_15_exp(i) EU_15_mid(i) EU_15_fit(:,2*i-1) EU_15_error(:,i)] = curve_fitting(EU_15_raw(:,2*i-1),EU_15_raw(:,2*i),x0,lb,ub);
    EU_15_fit(:,2*i) = EU_15_raw(:,2*i);
    if EU_15_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(EU_15_raw(:,2*i-1),EU_15_raw(:,2*i),x0,lb,ub);
        if fval < EU_15_fval(i);
            EU_15_fval(i) = fval; EU_15_exp(i) = exp; EU_15_mid(i) = mid;
            EU_15_fit(:,2*i-1) = fit;
        end
    end
end
end

Europe_Eastern_raw = [Europe_Eastern_c4_low_l_cum,Europe_Eastern_c4_low_l_sort(:,2),...
    Europe_Eastern_c4_high_l_cum,Europe_Eastern_c4_high_l_sort(:,2),...
    Europe_Eastern_c4_low_h_cum,Europe_Eastern_c4_low_h_sort(:,2),...
    Europe_Eastern_c4_high_h_cum,Europe_Eastern_c4_high_h_sort(:,2),...
    Europe_Eastern_c5_low_l_cum,Europe_Eastern_c5_low_l_sort(:,2),...
    Europe_Eastern_c5_high_l_cum,Europe_Eastern_c5_high_l_sort(:,2),...
    Europe_Eastern_c5_low_h_cum,Europe_Eastern_c5_low_h_sort(:,2),...
    Europe_Eastern_c5_high_h_cum,Europe_Eastern_c5_high_h_sort(:,2),...
    Europe_Eastern_c6_low_l_cum,Europe_Eastern_c6_low_l_sort(:,2),...
    Europe_Eastern_c6_high_l_cum,Europe_Eastern_c6_high_l_sort(:,2),...
    Europe_Eastern_c6_low_h_cum,Europe_Eastern_c6_low_h_sort(:,2),...
    Europe_Eastern_c6_high_h_cum,Europe_Eastern_c6_high_h_sort(:,2),...
    Europe_Eastern_c7_low_l_cum,Europe_Eastern_c7_low_l_sort(:,2),...
    Europe_Eastern_c7_high_l_cum,Europe_Eastern_c7_high_l_sort(:,2),...
    Europe_Eastern_c7_low_h_cum,Europe_Eastern_c7_low_h_sort(:,2),...
    Europe_Eastern_c7_high_h_cum,Europe_Eastern_c7_high_h_sort(:,2),...
    Europe_Eastern_c89_low_l_cum,Europe_Eastern_c89_low_l_sort(:,2),...
    Europe_Eastern_c89_high_l_cum,Europe_Eastern_c89_high_l_sort(:,2),...
    Europe_Eastern_c89_low_h_cum,Europe_Eastern_c89_low_h_sort(:,2),...
    Europe_Eastern_c89_high_h_cum,Europe_Eastern_c89_high_h_sort(:,2)];

Europe_Eastern_fval = zeros(1,size(Europe_Eastern_raw,2)/2);
Europe_Eastern_exp = Europe_Eastern_fval; Europe_Eastern_mid = Europe_Eastern_fval;
Europe_Eastern_fit = zeros(size(Europe_Eastern_raw));
Europe_Eastern_error = zeros(size(Europe_Eastern_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Europe_Eastern_raw,2)/2;
if Europe_Eastern_raw(:,2*i-1) == 0;
else
    [Europe_Eastern_fval(i) Europe_Eastern_exp(i) Europe_Eastern_mid(i) Europe_Eastern_fit(:,2*i-1) Europe_Eastern_error(:,i)] = curve_fitting(Europe_Eastern_raw(:,2*i-1),Europe_Eastern_raw(:,2*i),x0,lb,ub);
    Europe_Eastern_fit(:,2*i) = Europe_Eastern_raw(:,2*i);
    if Europe_Eastern_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Europe_Eastern_raw(:,2*i-1),Europe_Eastern_raw(:,2*i),x0,lb,ub);
        if fval < Europe_Eastern_fval(i);
            Europe_Eastern_fval(i) = fval; Europe_Eastern_exp(i) = exp; Europe_Eastern_mid(i) = mid;
            Europe_Eastern_fit(:,2*i-1) = fit;
        end
    end
end
end

Europe_Non_EU_raw = [Europe_Non_EU_c4_low_l_cum,Europe_Non_EU_c4_low_l_sort(:,2),...
    Europe_Non_EU_c4_high_l_cum,Europe_Non_EU_c4_high_l_sort(:,2),...
    Europe_Non_EU_c4_low_h_cum,Europe_Non_EU_c4_low_h_sort(:,2),...
    Europe_Non_EU_c4_high_h_cum,Europe_Non_EU_c4_high_h_sort(:,2),...
    Europe_Non_EU_c5_low_l_cum,Europe_Non_EU_c5_low_l_sort(:,2),...
    Europe_Non_EU_c5_high_l_cum,Europe_Non_EU_c5_high_l_sort(:,2),...
    Europe_Non_EU_c5_low_h_cum,Europe_Non_EU_c5_low_h_sort(:,2),...
    Europe_Non_EU_c5_high_h_cum,Europe_Non_EU_c5_high_h_sort(:,2),...
    Europe_Non_EU_c6_low_l_cum,Europe_Non_EU_c6_low_l_sort(:,2),...
    Europe_Non_EU_c6_high_l_cum,Europe_Non_EU_c6_high_l_sort(:,2),...
    Europe_Non_EU_c6_low_h_cum,Europe_Non_EU_c6_low_h_sort(:,2),...
    Europe_Non_EU_c6_high_h_cum,Europe_Non_EU_c6_high_h_sort(:,2),...
    Europe_Non_EU_c7_low_l_cum,Europe_Non_EU_c7_low_l_sort(:,2),...
    Europe_Non_EU_c7_high_l_cum,Europe_Non_EU_c7_high_l_sort(:,2),...
    Europe_Non_EU_c7_low_h_cum,Europe_Non_EU_c7_low_h_sort(:,2),...
    Europe_Non_EU_c7_high_h_cum,Europe_Non_EU_c7_high_h_sort(:,2),...
    Europe_Non_EU_c89_low_l_cum,Europe_Non_EU_c89_low_l_sort(:,2),...
    Europe_Non_EU_c89_high_l_cum,Europe_Non_EU_c89_high_l_sort(:,2),...
    Europe_Non_EU_c89_low_h_cum,Europe_Non_EU_c89_low_h_sort(:,2),...
    Europe_Non_EU_c89_high_h_cum,Europe_Non_EU_c89_high_h_sort(:,2)];

Europe_Non_EU_fval = zeros(1,size(Europe_Non_EU_raw,2)/2);
Europe_Non_EU_exp = Europe_Non_EU_fval; Europe_Non_EU_mid = Europe_Non_EU_fval;
Europe_Non_EU_fit = zeros(size(Europe_Non_EU_raw));
Europe_Non_EU_error = zeros(size(Europe_Non_EU_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Europe_Non_EU_raw,2)/2;
if Europe_Non_EU_raw(:,2*i-1) == 0;
else
    [Europe_Non_EU_fval(i) Europe_Non_EU_exp(i) Europe_Non_EU_mid(i) Europe_Non_EU_fit(:,2*i-1) Europe_Non_EU_error(:,i)] = curve_fitting(Europe_Non_EU_raw(:,2*i-1),Europe_Non_EU_raw(:,2*i),x0,lb,ub);
    Europe_Non_EU_fit(:,2*i) = Europe_Non_EU_raw(:,2*i);
    if Europe_Non_EU_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Europe_Non_EU_raw(:,2*i-1),Europe_Non_EU_raw(:,2*i),x0,lb,ub);
        if fval < Europe_Non_EU_fval(i);
            Europe_Non_EU_fval(i) = fval; Europe_Non_EU_exp(i) = exp; Europe_Non_EU_mid(i) = mid;
            Europe_Non_EU_fit(:,2*i-1) = fit;
        end
    end
end
end

European_Free_Trade_Association_raw = [European_Free_Trade_Association_c4_low_l_cum,European_Free_Trade_Association_c4_low_l_sort(:,2),...
    European_Free_Trade_Association_c4_high_l_cum,European_Free_Trade_Association_c4_high_l_sort(:,2),...
    European_Free_Trade_Association_c4_low_h_cum,European_Free_Trade_Association_c4_low_h_sort(:,2),...
    European_Free_Trade_Association_c4_high_h_cum,European_Free_Trade_Association_c4_high_h_sort(:,2),...
    European_Free_Trade_Association_c5_low_l_cum,European_Free_Trade_Association_c5_low_l_sort(:,2),...
    European_Free_Trade_Association_c5_high_l_cum,European_Free_Trade_Association_c5_high_l_sort(:,2),...
    European_Free_Trade_Association_c5_low_h_cum,European_Free_Trade_Association_c5_low_h_sort(:,2),...
    European_Free_Trade_Association_c5_high_h_cum,European_Free_Trade_Association_c5_high_h_sort(:,2),...
    European_Free_Trade_Association_c6_low_l_cum,European_Free_Trade_Association_c6_low_l_sort(:,2),...
    European_Free_Trade_Association_c6_high_l_cum,European_Free_Trade_Association_c6_high_l_sort(:,2),...
    European_Free_Trade_Association_c6_low_h_cum,European_Free_Trade_Association_c6_low_h_sort(:,2),...
    European_Free_Trade_Association_c6_high_h_cum,European_Free_Trade_Association_c6_high_h_sort(:,2),...
    European_Free_Trade_Association_c7_low_l_cum,European_Free_Trade_Association_c7_low_l_sort(:,2),...
    European_Free_Trade_Association_c7_high_l_cum,European_Free_Trade_Association_c7_high_l_sort(:,2),...
    European_Free_Trade_Association_c7_low_h_cum,European_Free_Trade_Association_c7_low_h_sort(:,2),...
    European_Free_Trade_Association_c7_high_h_cum,European_Free_Trade_Association_c7_high_h_sort(:,2),...
    European_Free_Trade_Association_c89_low_l_cum,European_Free_Trade_Association_c89_low_l_sort(:,2),...
    European_Free_Trade_Association_c89_high_l_cum,European_Free_Trade_Association_c89_high_l_sort(:,2),...
    European_Free_Trade_Association_c89_low_h_cum,European_Free_Trade_Association_c89_low_h_sort(:,2),...
    European_Free_Trade_Association_c89_high_h_cum,European_Free_Trade_Association_c89_high_h_sort(:,2)];

European_Free_Trade_Association_fval = zeros(1,size(European_Free_Trade_Association_raw,2)/2);
European_Free_Trade_Association_exp = European_Free_Trade_Association_fval; European_Free_Trade_Association_mid = European_Free_Trade_Association_fval;
European_Free_Trade_Association_fit = zeros(size(European_Free_Trade_Association_raw));
European_Free_Trade_Association_error = zeros(size(European_Free_Trade_Association_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(European_Free_Trade_Association_raw,2)/2;
if European_Free_Trade_Association_raw(:,2*i-1) == 0;
else
    [European_Free_Trade_Association_fval(i) European_Free_Trade_Association_exp(i) European_Free_Trade_Association_mid(i) European_Free_Trade_Association_fit(:,2*i-1) European_Free_Trade_Association_error(:,i)] = curve_fitting(European_Free_Trade_Association_raw(:,2*i-1),European_Free_Trade_Association_raw(:,2*i),x0,lb,ub);
    European_Free_Trade_Association_fit(:,2*i) = European_Free_Trade_Association_raw(:,2*i);
    if European_Free_Trade_Association_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(European_Free_Trade_Association_raw(:,2*i-1),European_Free_Trade_Association_raw(:,2*i),x0,lb,ub);
        if fval < European_Free_Trade_Association_fval(i);
            European_Free_Trade_Association_fval(i) = fval; European_Free_Trade_Association_exp(i) = exp; European_Free_Trade_Association_mid(i) = mid;
            European_Free_Trade_Association_fit(:,2*i-1) = fit;
        end
    end
end
end

India_raw = [India_c4_low_l_cum,India_c4_low_l_sort(:,2),...
    India_c4_high_l_cum,India_c4_high_l_sort(:,2),...
    India_c4_low_h_cum,India_c4_low_h_sort(:,2),...
    India_c4_high_h_cum,India_c4_high_h_sort(:,2),...
    India_c5_low_l_cum,India_c5_low_l_sort(:,2),...
    India_c5_high_l_cum,India_c5_high_l_sort(:,2),...
    India_c5_low_h_cum,India_c5_low_h_sort(:,2),...
    India_c5_high_h_cum,India_c5_high_h_sort(:,2),...
    India_c6_low_l_cum,India_c6_low_l_sort(:,2),...
    India_c6_high_l_cum,India_c6_high_l_sort(:,2),...
    India_c6_low_h_cum,India_c6_low_h_sort(:,2),...
    India_c6_high_h_cum,India_c6_high_h_sort(:,2),...
    India_c7_low_l_cum,India_c7_low_l_sort(:,2),...
    India_c7_high_l_cum,India_c7_high_l_sort(:,2),...
    India_c7_low_h_cum,India_c7_low_h_sort(:,2),...
    India_c7_high_h_cum,India_c7_high_h_sort(:,2),...
    India_c89_low_l_cum,India_c89_low_l_sort(:,2),...
    India_c89_high_l_cum,India_c89_high_l_sort(:,2),...
    India_c89_low_h_cum,India_c89_low_h_sort(:,2),...
    India_c89_high_h_cum,India_c89_high_h_sort(:,2)];

India_fval = zeros(1,size(India_raw,2)/2);
India_exp = India_fval; India_mid = India_fval;
India_fit = zeros(size(India_raw));
India_error = zeros(size(India_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(India_raw,2)/2;
if India_raw(:,2*i-1) == 0;
else
    [India_fval(i) India_exp(i) India_mid(i) India_fit(:,2*i-1) India_error(:,i)] = curve_fitting(India_raw(:,2*i-1),India_raw(:,2*i),x0,lb,ub);
    India_fit(:,2*i) = India_raw(:,2*i);
    if India_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(India_raw(:,2*i-1),India_raw(:,2*i),x0,lb,ub);
        if fval < India_fval(i);
            India_fval(i) = fval; India_exp(i) = exp; India_mid(i) = mid;
            India_fit(:,2*i-1) = fit;
        end
    end
end
end

Indonesia_raw = [Indonesia_c4_low_l_cum,Indonesia_c4_low_l_sort(:,2),...
    Indonesia_c4_high_l_cum,Indonesia_c4_high_l_sort(:,2),...
    Indonesia_c4_low_h_cum,Indonesia_c4_low_h_sort(:,2),...
    Indonesia_c4_high_h_cum,Indonesia_c4_high_h_sort(:,2),...
    Indonesia_c5_low_l_cum,Indonesia_c5_low_l_sort(:,2),...
    Indonesia_c5_high_l_cum,Indonesia_c5_high_l_sort(:,2),...
    Indonesia_c5_low_h_cum,Indonesia_c5_low_h_sort(:,2),...
    Indonesia_c5_high_h_cum,Indonesia_c5_high_h_sort(:,2),...
    Indonesia_c6_low_l_cum,Indonesia_c6_low_l_sort(:,2),...
    Indonesia_c6_high_l_cum,Indonesia_c6_high_l_sort(:,2),...
    Indonesia_c6_low_h_cum,Indonesia_c6_low_h_sort(:,2),...
    Indonesia_c6_high_h_cum,Indonesia_c6_high_h_sort(:,2),...
    Indonesia_c7_low_l_cum,Indonesia_c7_low_l_sort(:,2),...
    Indonesia_c7_high_l_cum,Indonesia_c7_high_l_sort(:,2),...
    Indonesia_c7_low_h_cum,Indonesia_c7_low_h_sort(:,2),...
    Indonesia_c7_high_h_cum,Indonesia_c7_high_h_sort(:,2),...
    Indonesia_c89_low_l_cum,Indonesia_c89_low_l_sort(:,2),...
    Indonesia_c89_high_l_cum,Indonesia_c89_high_l_sort(:,2),...
    Indonesia_c89_low_h_cum,Indonesia_c89_low_h_sort(:,2),...
    Indonesia_c89_high_h_cum,Indonesia_c89_high_h_sort(:,2)];

Indonesia_fval = zeros(1,size(Indonesia_raw,2)/2);
Indonesia_exp = Indonesia_fval; Indonesia_mid = Indonesia_fval;
Indonesia_fit = zeros(size(Indonesia_raw));
Indonesia_error = zeros(size(Indonesia_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Indonesia_raw,2)/2;
if Indonesia_raw(:,2*i-1) == 0;
else
    [Indonesia_fval(i) Indonesia_exp(i) Indonesia_mid(i) Indonesia_fit(:,2*i-1) Indonesia_error(:,i)] = curve_fitting(Indonesia_raw(:,2*i-1),Indonesia_raw(:,2*i),x0,lb,ub);
    Indonesia_fit(:,2*i) = Indonesia_raw(:,2*i);
    if Indonesia_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Indonesia_raw(:,2*i-1),Indonesia_raw(:,2*i),x0,lb,ub);
        if fval < Indonesia_fval(i);
            Indonesia_fval(i) = fval; Indonesia_exp(i) = exp; Indonesia_mid(i) = mid;
            Indonesia_fit(:,2*i-1) = fit;
        end
    end
end
end

Japan_raw = [Japan_c4_low_l_cum,Japan_c4_low_l_sort(:,2),...
    Japan_c4_high_l_cum,Japan_c4_high_l_sort(:,2),...
    Japan_c4_low_h_cum,Japan_c4_low_h_sort(:,2),...
    Japan_c4_high_h_cum,Japan_c4_high_h_sort(:,2),...
    Japan_c5_low_l_cum,Japan_c5_low_l_sort(:,2),...
    Japan_c5_high_l_cum,Japan_c5_high_l_sort(:,2),...
    Japan_c5_low_h_cum,Japan_c5_low_h_sort(:,2),...
    Japan_c5_high_h_cum,Japan_c5_high_h_sort(:,2),...
    Japan_c6_low_l_cum,Japan_c6_low_l_sort(:,2),...
    Japan_c6_high_l_cum,Japan_c6_high_l_sort(:,2),...
    Japan_c6_low_h_cum,Japan_c6_low_h_sort(:,2),...
    Japan_c6_high_h_cum,Japan_c6_high_h_sort(:,2),...
    Japan_c7_low_l_cum,Japan_c7_low_l_sort(:,2),...
    Japan_c7_high_l_cum,Japan_c7_high_l_sort(:,2),...
    Japan_c7_low_h_cum,Japan_c7_low_h_sort(:,2),...
    Japan_c7_high_h_cum,Japan_c7_high_h_sort(:,2),...
    Japan_c89_low_l_cum,Japan_c89_low_l_sort(:,2),...
    Japan_c89_high_l_cum,Japan_c89_high_l_sort(:,2),...
    Japan_c89_low_h_cum,Japan_c89_low_h_sort(:,2),...
    Japan_c89_high_h_cum,Japan_c89_high_h_sort(:,2)];

Japan_fval = zeros(1,size(Japan_raw,2)/2);
Japan_exp = Japan_fval; Japan_mid = Japan_fval;
Japan_fit = zeros(size(Japan_raw));
Japan_error = zeros(size(Japan_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Japan_raw,2)/2;
if Japan_raw(:,2*i-1) == 0;
else
    [Japan_fval(i) Japan_exp(i) Japan_mid(i) Japan_fit(:,2*i-1) Japan_error(:,i)] = curve_fitting(Japan_raw(:,2*i-1),Japan_raw(:,2*i),x0,lb,ub);
    Japan_fit(:,2*i) = Japan_raw(:,2*i);
    if Japan_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Japan_raw(:,2*i-1),Japan_raw(:,2*i),x0,lb,ub);
        if fval < Japan_fval(i);
            Japan_fval(i) = fval; Japan_exp(i) = exp; Japan_mid(i) = mid;
            Japan_fit(:,2*i-1) = fit;
        end
    end
end
end

Mexico_raw = [Mexico_c4_low_l_cum,Mexico_c4_low_l_sort(:,2),...
    Mexico_c4_high_l_cum,Mexico_c4_high_l_sort(:,2),...
    Mexico_c4_low_h_cum,Mexico_c4_low_h_sort(:,2),...
    Mexico_c4_high_h_cum,Mexico_c4_high_h_sort(:,2),...
    Mexico_c5_low_l_cum,Mexico_c5_low_l_sort(:,2),...
    Mexico_c5_high_l_cum,Mexico_c5_high_l_sort(:,2),...
    Mexico_c5_low_h_cum,Mexico_c5_low_h_sort(:,2),...
    Mexico_c5_high_h_cum,Mexico_c5_high_h_sort(:,2),...
    Mexico_c6_low_l_cum,Mexico_c6_low_l_sort(:,2),...
    Mexico_c6_high_l_cum,Mexico_c6_high_l_sort(:,2),...
    Mexico_c6_low_h_cum,Mexico_c6_low_h_sort(:,2),...
    Mexico_c6_high_h_cum,Mexico_c6_high_h_sort(:,2),...
    Mexico_c7_low_l_cum,Mexico_c7_low_l_sort(:,2),...
    Mexico_c7_high_l_cum,Mexico_c7_high_l_sort(:,2),...
    Mexico_c7_low_h_cum,Mexico_c7_low_h_sort(:,2),...
    Mexico_c7_high_h_cum,Mexico_c7_high_h_sort(:,2),...
    Mexico_c89_low_l_cum,Mexico_c89_low_l_sort(:,2),...
    Mexico_c89_high_l_cum,Mexico_c89_high_l_sort(:,2),...
    Mexico_c89_low_h_cum,Mexico_c89_low_h_sort(:,2),...
    Mexico_c89_high_h_cum,Mexico_c89_high_h_sort(:,2)];

Mexico_fval = zeros(1,size(Mexico_raw,2)/2);
Mexico_exp = Mexico_fval; Mexico_mid = Mexico_fval;
Mexico_fit = zeros(size(Mexico_raw));
Mexico_error = zeros(size(Mexico_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Mexico_raw,2)/2;
if Mexico_raw(:,2*i-1) == 0;
else
    [Mexico_fval(i) Mexico_exp(i) Mexico_mid(i) Mexico_fit(:,2*i-1) Mexico_error(:,i)] = curve_fitting(Mexico_raw(:,2*i-1),Mexico_raw(:,2*i),x0,lb,ub);
    Mexico_fit(:,2*i) = Mexico_raw(:,2*i);
    if Mexico_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Mexico_raw(:,2*i-1),Mexico_raw(:,2*i),x0,lb,ub);
        if fval < Mexico_fval(i);
            Mexico_fval(i) = fval; Mexico_exp(i) = exp; Mexico_mid(i) = mid;
            Mexico_fit(:,2*i-1) = fit;
        end
    end
end
end

Middle_East_raw = [Middle_East_c4_low_l_cum,Middle_East_c4_low_l_sort(:,2),...
    Middle_East_c4_high_l_cum,Middle_East_c4_high_l_sort(:,2),...
    Middle_East_c4_low_h_cum,Middle_East_c4_low_h_sort(:,2),...
    Middle_East_c4_high_h_cum,Middle_East_c4_high_h_sort(:,2),...
    Middle_East_c5_low_l_cum,Middle_East_c5_low_l_sort(:,2),...
    Middle_East_c5_high_l_cum,Middle_East_c5_high_l_sort(:,2),...
    Middle_East_c5_low_h_cum,Middle_East_c5_low_h_sort(:,2),...
    Middle_East_c5_high_h_cum,Middle_East_c5_high_h_sort(:,2),...
    Middle_East_c6_low_l_cum,Middle_East_c6_low_l_sort(:,2),...
    Middle_East_c6_high_l_cum,Middle_East_c6_high_l_sort(:,2),...
    Middle_East_c6_low_h_cum,Middle_East_c6_low_h_sort(:,2),...
    Middle_East_c6_high_h_cum,Middle_East_c6_high_h_sort(:,2),...
    Middle_East_c7_low_l_cum,Middle_East_c7_low_l_sort(:,2),...
    Middle_East_c7_high_l_cum,Middle_East_c7_high_l_sort(:,2),...
    Middle_East_c7_low_h_cum,Middle_East_c7_low_h_sort(:,2),...
    Middle_East_c7_high_h_cum,Middle_East_c7_high_h_sort(:,2),...
    Middle_East_c89_low_l_cum,Middle_East_c89_low_l_sort(:,2),...
    Middle_East_c89_high_l_cum,Middle_East_c89_high_l_sort(:,2),...
    Middle_East_c89_low_h_cum,Middle_East_c89_low_h_sort(:,2),...
    Middle_East_c89_high_h_cum,Middle_East_c89_high_h_sort(:,2)];

Middle_East_fval = zeros(1,size(Middle_East_raw,2)/2);
Middle_East_exp = Middle_East_fval; Middle_East_mid = Middle_East_fval;
Middle_East_fit = zeros(size(Middle_East_raw));
Middle_East_error = zeros(size(Middle_East_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Middle_East_raw,2)/2;
if Middle_East_raw(:,2*i-1) == 0;
else
    [Middle_East_fval(i) Middle_East_exp(i) Middle_East_mid(i) Middle_East_fit(:,2*i-1) Middle_East_error(:,i)] = curve_fitting(Middle_East_raw(:,2*i-1),Middle_East_raw(:,2*i),x0,lb,ub);
    Middle_East_fit(:,2*i) = Middle_East_raw(:,2*i);
    if Middle_East_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Middle_East_raw(:,2*i-1),Middle_East_raw(:,2*i),x0,lb,ub);
        if fval < Middle_East_fval(i);
            Middle_East_fval(i) = fval; Middle_East_exp(i) = exp; Middle_East_mid(i) = mid;
            Middle_East_fit(:,2*i-1) = fit;
        end
    end
end
end

Pakistan_raw = [Pakistan_c4_low_l_cum,Pakistan_c4_low_l_sort(:,2),...
    Pakistan_c4_high_l_cum,Pakistan_c4_high_l_sort(:,2),...
    Pakistan_c4_low_h_cum,Pakistan_c4_low_h_sort(:,2),...
    Pakistan_c4_high_h_cum,Pakistan_c4_high_h_sort(:,2),...
    Pakistan_c5_low_l_cum,Pakistan_c5_low_l_sort(:,2),...
    Pakistan_c5_high_l_cum,Pakistan_c5_high_l_sort(:,2),...
    Pakistan_c5_low_h_cum,Pakistan_c5_low_h_sort(:,2),...
    Pakistan_c5_high_h_cum,Pakistan_c5_high_h_sort(:,2),...
    Pakistan_c6_low_l_cum,Pakistan_c6_low_l_sort(:,2),...
    Pakistan_c6_high_l_cum,Pakistan_c6_high_l_sort(:,2),...
    Pakistan_c6_low_h_cum,Pakistan_c6_low_h_sort(:,2),...
    Pakistan_c6_high_h_cum,Pakistan_c6_high_h_sort(:,2),...
    Pakistan_c7_low_l_cum,Pakistan_c7_low_l_sort(:,2),...
    Pakistan_c7_high_l_cum,Pakistan_c7_high_l_sort(:,2),...
    Pakistan_c7_low_h_cum,Pakistan_c7_low_h_sort(:,2),...
    Pakistan_c7_high_h_cum,Pakistan_c7_high_h_sort(:,2),...
    Pakistan_c89_low_l_cum,Pakistan_c89_low_l_sort(:,2),...
    Pakistan_c89_high_l_cum,Pakistan_c89_high_l_sort(:,2),...
    Pakistan_c89_low_h_cum,Pakistan_c89_low_h_sort(:,2),...
    Pakistan_c89_high_h_cum,Pakistan_c89_high_h_sort(:,2)];

Pakistan_fval = zeros(1,size(Pakistan_raw,2)/2);
Pakistan_exp = Pakistan_fval; Pakistan_mid = Pakistan_fval;
Pakistan_fit = zeros(size(Pakistan_raw));
Pakistan_error = zeros(size(Pakistan_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Pakistan_raw,2)/2;
if Pakistan_raw(:,2*i-1) == 0;
else
    [Pakistan_fval(i) Pakistan_exp(i) Pakistan_mid(i) Pakistan_fit(:,2*i-1) Pakistan_error(:,i)] = curve_fitting(Pakistan_raw(:,2*i-1),Pakistan_raw(:,2*i),x0,lb,ub);
    Pakistan_fit(:,2*i) = Pakistan_raw(:,2*i);
    if Pakistan_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Pakistan_raw(:,2*i-1),Pakistan_raw(:,2*i),x0,lb,ub);
        if fval < Pakistan_fval(i);
            Pakistan_fval(i) = fval; Pakistan_exp(i) = exp; Pakistan_mid(i) = mid;
            Pakistan_fit(:,2*i-1) = fit;
        end
    end
end
end

Russia_raw = [Russia_c4_low_l_cum,Russia_c4_low_l_sort(:,2),...
    Russia_c4_high_l_cum,Russia_c4_high_l_sort(:,2),...
    Russia_c4_low_h_cum,Russia_c4_low_h_sort(:,2),...
    Russia_c4_high_h_cum,Russia_c4_high_h_sort(:,2),...
    Russia_c5_low_l_cum,Russia_c5_low_l_sort(:,2),...
    Russia_c5_high_l_cum,Russia_c5_high_l_sort(:,2),...
    Russia_c5_low_h_cum,Russia_c5_low_h_sort(:,2),...
    Russia_c5_high_h_cum,Russia_c5_high_h_sort(:,2),...
    Russia_c6_low_l_cum,Russia_c6_low_l_sort(:,2),...
    Russia_c6_high_l_cum,Russia_c6_high_l_sort(:,2),...
    Russia_c6_low_h_cum,Russia_c6_low_h_sort(:,2),...
    Russia_c6_high_h_cum,Russia_c6_high_h_sort(:,2),...
    Russia_c7_low_l_cum,Russia_c7_low_l_sort(:,2),...
    Russia_c7_high_l_cum,Russia_c7_high_l_sort(:,2),...
    Russia_c7_low_h_cum,Russia_c7_low_h_sort(:,2),...
    Russia_c7_high_h_cum,Russia_c7_high_h_sort(:,2),...
    Russia_c89_low_l_cum,Russia_c89_low_l_sort(:,2),...
    Russia_c89_high_l_cum,Russia_c89_high_l_sort(:,2),...
    Russia_c89_low_h_cum,Russia_c89_low_h_sort(:,2),...
    Russia_c89_high_h_cum,Russia_c89_high_h_sort(:,2)];

Russia_fval = zeros(1,size(Russia_raw,2)/2);
Russia_exp = Russia_fval; Russia_mid = Russia_fval;
Russia_fit = zeros(size(Russia_raw));
Russia_error = zeros(size(Russia_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Russia_raw,2)/2;
if Russia_raw(:,2*i-1) == 0;
else
    [Russia_fval(i) Russia_exp(i) Russia_mid(i) Russia_fit(:,2*i-1) Russia_error(:,i)] = curve_fitting(Russia_raw(:,2*i-1),Russia_raw(:,2*i),x0,lb,ub);
    Russia_fit(:,2*i) = Russia_raw(:,2*i);
    if Russia_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Russia_raw(:,2*i-1),Russia_raw(:,2*i),x0,lb,ub);
        if fval < Russia_fval(i);
            Russia_fval(i) = fval; Russia_exp(i) = exp; Russia_mid(i) = mid;
            Russia_fit(:,2*i-1) = fit;
        end
    end
end
end

South_Africa_raw = [South_Africa_c4_low_l_cum,South_Africa_c4_low_l_sort(:,2),...
    South_Africa_c4_high_l_cum,South_Africa_c4_high_l_sort(:,2),...
    South_Africa_c4_low_h_cum,South_Africa_c4_low_h_sort(:,2),...
    South_Africa_c4_high_h_cum,South_Africa_c4_high_h_sort(:,2),...
    South_Africa_c5_low_l_cum,South_Africa_c5_low_l_sort(:,2),...
    South_Africa_c5_high_l_cum,South_Africa_c5_high_l_sort(:,2),...
    South_Africa_c5_low_h_cum,South_Africa_c5_low_h_sort(:,2),...
    South_Africa_c5_high_h_cum,South_Africa_c5_high_h_sort(:,2),...
    South_Africa_c6_low_l_cum,South_Africa_c6_low_l_sort(:,2),...
    South_Africa_c6_high_l_cum,South_Africa_c6_high_l_sort(:,2),...
    South_Africa_c6_low_h_cum,South_Africa_c6_low_h_sort(:,2),...
    South_Africa_c6_high_h_cum,South_Africa_c6_high_h_sort(:,2),...
    South_Africa_c7_low_l_cum,South_Africa_c7_low_l_sort(:,2),...
    South_Africa_c7_high_l_cum,South_Africa_c7_high_l_sort(:,2),...
    South_Africa_c7_low_h_cum,South_Africa_c7_low_h_sort(:,2),...
    South_Africa_c7_high_h_cum,South_Africa_c7_high_h_sort(:,2),...
    South_Africa_c89_low_l_cum,South_Africa_c89_low_l_sort(:,2),...
    South_Africa_c89_high_l_cum,South_Africa_c89_high_l_sort(:,2),...
    South_Africa_c89_low_h_cum,South_Africa_c89_low_h_sort(:,2),...
    South_Africa_c89_high_h_cum,South_Africa_c89_high_h_sort(:,2)];

South_Africa_fval = zeros(1,size(South_Africa_raw,2)/2);
South_Africa_exp = South_Africa_fval; South_Africa_mid = South_Africa_fval;
South_Africa_fit = zeros(size(South_Africa_raw));
South_Africa_error = zeros(size(South_Africa_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(South_Africa_raw,2)/2;
if South_Africa_raw(:,2*i-1) == 0;
else
    [South_Africa_fval(i) South_Africa_exp(i) South_Africa_mid(i) South_Africa_fit(:,2*i-1) South_Africa_error(:,i)] = curve_fitting(South_Africa_raw(:,2*i-1),South_Africa_raw(:,2*i),x0,lb,ub);
    South_Africa_fit(:,2*i) = South_Africa_raw(:,2*i);
    if South_Africa_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(South_Africa_raw(:,2*i-1),South_Africa_raw(:,2*i),x0,lb,ub);
        if fval < South_Africa_fval(i);
            South_Africa_fval(i) = fval; South_Africa_exp(i) = exp; South_Africa_mid(i) = mid;
            South_Africa_fit(:,2*i-1) = fit;
        end
    end
end
end

South_America_Northern_raw = [South_America_Northern_c4_low_l_cum,South_America_Northern_c4_low_l_sort(:,2),...
    South_America_Northern_c4_high_l_cum,South_America_Northern_c4_high_l_sort(:,2),...
    South_America_Northern_c4_low_h_cum,South_America_Northern_c4_low_h_sort(:,2),...
    South_America_Northern_c4_high_h_cum,South_America_Northern_c4_high_h_sort(:,2),...
    South_America_Northern_c5_low_l_cum,South_America_Northern_c5_low_l_sort(:,2),...
    South_America_Northern_c5_high_l_cum,South_America_Northern_c5_high_l_sort(:,2),...
    South_America_Northern_c5_low_h_cum,South_America_Northern_c5_low_h_sort(:,2),...
    South_America_Northern_c5_high_h_cum,South_America_Northern_c5_high_h_sort(:,2),...
    South_America_Northern_c6_low_l_cum,South_America_Northern_c6_low_l_sort(:,2),...
    South_America_Northern_c6_high_l_cum,South_America_Northern_c6_high_l_sort(:,2),...
    South_America_Northern_c6_low_h_cum,South_America_Northern_c6_low_h_sort(:,2),...
    South_America_Northern_c6_high_h_cum,South_America_Northern_c6_high_h_sort(:,2),...
    South_America_Northern_c7_low_l_cum,South_America_Northern_c7_low_l_sort(:,2),...
    South_America_Northern_c7_high_l_cum,South_America_Northern_c7_high_l_sort(:,2),...
    South_America_Northern_c7_low_h_cum,South_America_Northern_c7_low_h_sort(:,2),...
    South_America_Northern_c7_high_h_cum,South_America_Northern_c7_high_h_sort(:,2),...
    South_America_Northern_c89_low_l_cum,South_America_Northern_c89_low_l_sort(:,2),...
    South_America_Northern_c89_high_l_cum,South_America_Northern_c89_high_l_sort(:,2),...
    South_America_Northern_c89_low_h_cum,South_America_Northern_c89_low_h_sort(:,2),...
    South_America_Northern_c89_high_h_cum,South_America_Northern_c89_high_h_sort(:,2)];

South_America_Northern_fval = zeros(1,size(South_America_Northern_raw,2)/2);
South_America_Northern_exp = South_America_Northern_fval; South_America_Northern_mid = South_America_Northern_fval;
South_America_Northern_fit = zeros(size(South_America_Northern_raw));
South_America_Northern_error = zeros(size(South_America_Northern_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(South_America_Northern_raw,2)/2;
if South_America_Northern_raw(:,2*i-1) == 0;
else
    [South_America_Northern_fval(i) South_America_Northern_exp(i) South_America_Northern_mid(i) South_America_Northern_fit(:,2*i-1) South_America_Northern_error(:,i)] = curve_fitting(South_America_Northern_raw(:,2*i-1),South_America_Northern_raw(:,2*i),x0,lb,ub);
    South_America_Northern_fit(:,2*i) = South_America_Northern_raw(:,2*i);
    if South_America_Northern_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(South_America_Northern_raw(:,2*i-1),South_America_Northern_raw(:,2*i),x0,lb,ub);
        if fval < South_America_Northern_fval(i);
            South_America_Northern_fval(i) = fval; South_America_Northern_exp(i) = exp; South_America_Northern_mid(i) = mid;
            South_America_Northern_fit(:,2*i-1) = fit;
        end
    end
end
end

South_America_Southern_raw = [South_America_Southern_c4_low_l_cum,South_America_Southern_c4_low_l_sort(:,2),...
    South_America_Southern_c4_high_l_cum,South_America_Southern_c4_high_l_sort(:,2),...
    South_America_Southern_c4_low_h_cum,South_America_Southern_c4_low_h_sort(:,2),...
    South_America_Southern_c4_high_h_cum,South_America_Southern_c4_high_h_sort(:,2),...
    South_America_Southern_c5_low_l_cum,South_America_Southern_c5_low_l_sort(:,2),...
    South_America_Southern_c5_high_l_cum,South_America_Southern_c5_high_l_sort(:,2),...
    South_America_Southern_c5_low_h_cum,South_America_Southern_c5_low_h_sort(:,2),...
    South_America_Southern_c5_high_h_cum,South_America_Southern_c5_high_h_sort(:,2),...
    South_America_Southern_c6_low_l_cum,South_America_Southern_c6_low_l_sort(:,2),...
    South_America_Southern_c6_high_l_cum,South_America_Southern_c6_high_l_sort(:,2),...
    South_America_Southern_c6_low_h_cum,South_America_Southern_c6_low_h_sort(:,2),...
    South_America_Southern_c6_high_h_cum,South_America_Southern_c6_high_h_sort(:,2),...
    South_America_Southern_c7_low_l_cum,South_America_Southern_c7_low_l_sort(:,2),...
    South_America_Southern_c7_high_l_cum,South_America_Southern_c7_high_l_sort(:,2),...
    South_America_Southern_c7_low_h_cum,South_America_Southern_c7_low_h_sort(:,2),...
    South_America_Southern_c7_high_h_cum,South_America_Southern_c7_high_h_sort(:,2),...
    South_America_Southern_c89_low_l_cum,South_America_Southern_c89_low_l_sort(:,2),...
    South_America_Southern_c89_high_l_cum,South_America_Southern_c89_high_l_sort(:,2),...
    South_America_Southern_c89_low_h_cum,South_America_Southern_c89_low_h_sort(:,2),...
    South_America_Southern_c89_high_h_cum,South_America_Southern_c89_high_h_sort(:,2)];

South_America_Southern_fval = zeros(1,size(South_America_Southern_raw,2)/2);
South_America_Southern_exp = South_America_Southern_fval; South_America_Southern_mid = South_America_Southern_fval;
South_America_Southern_fit = zeros(size(South_America_Southern_raw));
South_America_Southern_error = zeros(size(South_America_Southern_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(South_America_Southern_raw,2)/2;
if South_America_Southern_raw(:,2*i-1) == 0;
else
    [South_America_Southern_fval(i) South_America_Southern_exp(i) South_America_Southern_mid(i) South_America_Southern_fit(:,2*i-1) South_America_Southern_error(:,i)] = curve_fitting(South_America_Southern_raw(:,2*i-1),South_America_Southern_raw(:,2*i),x0,lb,ub);
    South_America_Southern_fit(:,2*i) = South_America_Southern_raw(:,2*i);
    if South_America_Southern_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(South_America_Southern_raw(:,2*i-1),South_America_Southern_raw(:,2*i),x0,lb,ub);
        if fval < South_America_Southern_fval(i);
            South_America_Southern_fval(i) = fval; South_America_Southern_exp(i) = exp; South_America_Southern_mid(i) = mid;
            South_America_Southern_fit(:,2*i-1) = fit;
        end
    end
end
end

South_Asia_raw = [South_Asia_c4_low_l_cum,South_Asia_c4_low_l_sort(:,2),...
    South_Asia_c4_high_l_cum,South_Asia_c4_high_l_sort(:,2),...
    South_Asia_c4_low_h_cum,South_Asia_c4_low_h_sort(:,2),...
    South_Asia_c4_high_h_cum,South_Asia_c4_high_h_sort(:,2),...
    South_Asia_c5_low_l_cum,South_Asia_c5_low_l_sort(:,2),...
    South_Asia_c5_high_l_cum,South_Asia_c5_high_l_sort(:,2),...
    South_Asia_c5_low_h_cum,South_Asia_c5_low_h_sort(:,2),...
    South_Asia_c5_high_h_cum,South_Asia_c5_high_h_sort(:,2),...
    South_Asia_c6_low_l_cum,South_Asia_c6_low_l_sort(:,2),...
    South_Asia_c6_high_l_cum,South_Asia_c6_high_l_sort(:,2),...
    South_Asia_c6_low_h_cum,South_Asia_c6_low_h_sort(:,2),...
    South_Asia_c6_high_h_cum,South_Asia_c6_high_h_sort(:,2),...
    South_Asia_c7_low_l_cum,South_Asia_c7_low_l_sort(:,2),...
    South_Asia_c7_high_l_cum,South_Asia_c7_high_l_sort(:,2),...
    South_Asia_c7_low_h_cum,South_Asia_c7_low_h_sort(:,2),...
    South_Asia_c7_high_h_cum,South_Asia_c7_high_h_sort(:,2),...
    South_Asia_c89_low_l_cum,South_Asia_c89_low_l_sort(:,2),...
    South_Asia_c89_high_l_cum,South_Asia_c89_high_l_sort(:,2),...
    South_Asia_c89_low_h_cum,South_Asia_c89_low_h_sort(:,2),...
    South_Asia_c89_high_h_cum,South_Asia_c89_high_h_sort(:,2)];

South_Asia_fval = zeros(1,size(South_Asia_raw,2)/2);
South_Asia_exp = South_Asia_fval; South_Asia_mid = South_Asia_fval;
South_Asia_fit = zeros(size(South_Asia_raw));
South_Asia_error = zeros(size(South_Asia_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(South_Asia_raw,2)/2;
if South_Asia_raw(:,2*i-1) == 0;
else
    [South_Asia_fval(i) South_Asia_exp(i) South_Asia_mid(i) South_Asia_fit(:,2*i-1) South_Asia_error(:,i)] = curve_fitting(South_Asia_raw(:,2*i-1),South_Asia_raw(:,2*i),x0,lb,ub);
    South_Asia_fit(:,2*i) = South_Asia_raw(:,2*i);
    if South_Asia_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(South_Asia_raw(:,2*i-1),South_Asia_raw(:,2*i),x0,lb,ub);
        if fval < South_Asia_fval(i);
            South_Asia_fval(i) = fval; South_Asia_exp(i) = exp; South_Asia_mid(i) = mid;
            South_Asia_fit(:,2*i-1) = fit;
        end
    end
end
end

South_Korea_raw = [South_Korea_c4_low_l_cum,South_Korea_c4_low_l_sort(:,2),...
    South_Korea_c4_high_l_cum,South_Korea_c4_high_l_sort(:,2),...
    South_Korea_c4_low_h_cum,South_Korea_c4_low_h_sort(:,2),...
    South_Korea_c4_high_h_cum,South_Korea_c4_high_h_sort(:,2),...
    South_Korea_c5_low_l_cum,South_Korea_c5_low_l_sort(:,2),...
    South_Korea_c5_high_l_cum,South_Korea_c5_high_l_sort(:,2),...
    South_Korea_c5_low_h_cum,South_Korea_c5_low_h_sort(:,2),...
    South_Korea_c5_high_h_cum,South_Korea_c5_high_h_sort(:,2),...
    South_Korea_c6_low_l_cum,South_Korea_c6_low_l_sort(:,2),...
    South_Korea_c6_high_l_cum,South_Korea_c6_high_l_sort(:,2),...
    South_Korea_c6_low_h_cum,South_Korea_c6_low_h_sort(:,2),...
    South_Korea_c6_high_h_cum,South_Korea_c6_high_h_sort(:,2),...
    South_Korea_c7_low_l_cum,South_Korea_c7_low_l_sort(:,2),...
    South_Korea_c7_high_l_cum,South_Korea_c7_high_l_sort(:,2),...
    South_Korea_c7_low_h_cum,South_Korea_c7_low_h_sort(:,2),...
    South_Korea_c7_high_h_cum,South_Korea_c7_high_h_sort(:,2),...
    South_Korea_c89_low_l_cum,South_Korea_c89_low_l_sort(:,2),...
    South_Korea_c89_high_l_cum,South_Korea_c89_high_l_sort(:,2),...
    South_Korea_c89_low_h_cum,South_Korea_c89_low_h_sort(:,2),...
    South_Korea_c89_high_h_cum,South_Korea_c89_high_h_sort(:,2)];

South_Korea_fval = zeros(1,size(South_Korea_raw,2)/2);
South_Korea_exp = South_Korea_fval; South_Korea_mid = South_Korea_fval;
South_Korea_fit = zeros(size(South_Korea_raw));
South_Korea_error = zeros(size(South_Korea_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(South_Korea_raw,2)/2;
if South_Korea_raw(:,2*i-1) == 0;
else
    [South_Korea_fval(i) South_Korea_exp(i) South_Korea_mid(i) South_Korea_fit(:,2*i-1) South_Korea_error(:,i)] = curve_fitting(South_Korea_raw(:,2*i-1),South_Korea_raw(:,2*i),x0,lb,ub);
    South_Korea_fit(:,2*i) = South_Korea_raw(:,2*i);
    if South_Korea_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(South_Korea_raw(:,2*i-1),South_Korea_raw(:,2*i),x0,lb,ub);
        if fval < South_Korea_fval(i);
            South_Korea_fval(i) = fval; South_Korea_exp(i) = exp; South_Korea_mid(i) = mid;
            South_Korea_fit(:,2*i-1) = fit;
        end
    end
end
end

Southeast_Asia_raw = [Southeast_Asia_c4_low_l_cum,Southeast_Asia_c4_low_l_sort(:,2),...
    Southeast_Asia_c4_high_l_cum,Southeast_Asia_c4_high_l_sort(:,2),...
    Southeast_Asia_c4_low_h_cum,Southeast_Asia_c4_low_h_sort(:,2),...
    Southeast_Asia_c4_high_h_cum,Southeast_Asia_c4_high_h_sort(:,2),...
    Southeast_Asia_c5_low_l_cum,Southeast_Asia_c5_low_l_sort(:,2),...
    Southeast_Asia_c5_high_l_cum,Southeast_Asia_c5_high_l_sort(:,2),...
    Southeast_Asia_c5_low_h_cum,Southeast_Asia_c5_low_h_sort(:,2),...
    Southeast_Asia_c5_high_h_cum,Southeast_Asia_c5_high_h_sort(:,2),...
    Southeast_Asia_c6_low_l_cum,Southeast_Asia_c6_low_l_sort(:,2),...
    Southeast_Asia_c6_high_l_cum,Southeast_Asia_c6_high_l_sort(:,2),...
    Southeast_Asia_c6_low_h_cum,Southeast_Asia_c6_low_h_sort(:,2),...
    Southeast_Asia_c6_high_h_cum,Southeast_Asia_c6_high_h_sort(:,2),...
    Southeast_Asia_c7_low_l_cum,Southeast_Asia_c7_low_l_sort(:,2),...
    Southeast_Asia_c7_high_l_cum,Southeast_Asia_c7_high_l_sort(:,2),...
    Southeast_Asia_c7_low_h_cum,Southeast_Asia_c7_low_h_sort(:,2),...
    Southeast_Asia_c7_high_h_cum,Southeast_Asia_c7_high_h_sort(:,2),...
    Southeast_Asia_c89_low_l_cum,Southeast_Asia_c89_low_l_sort(:,2),...
    Southeast_Asia_c89_high_l_cum,Southeast_Asia_c89_high_l_sort(:,2),...
    Southeast_Asia_c89_low_h_cum,Southeast_Asia_c89_low_h_sort(:,2),...
    Southeast_Asia_c89_high_h_cum,Southeast_Asia_c89_high_h_sort(:,2)];

Southeast_Asia_fval = zeros(1,size(Southeast_Asia_raw,2)/2);
Southeast_Asia_exp = Southeast_Asia_fval; Southeast_Asia_mid = Southeast_Asia_fval;
Southeast_Asia_fit = zeros(size(Southeast_Asia_raw));
Southeast_Asia_error = zeros(size(Southeast_Asia_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Southeast_Asia_raw,2)/2;
if Southeast_Asia_raw(:,2*i-1) == 0;
else
    [Southeast_Asia_fval(i) Southeast_Asia_exp(i) Southeast_Asia_mid(i) Southeast_Asia_fit(:,2*i-1) Southeast_Asia_error(:,i)] = curve_fitting(Southeast_Asia_raw(:,2*i-1),Southeast_Asia_raw(:,2*i),x0,lb,ub);
    Southeast_Asia_fit(:,2*i) = Southeast_Asia_raw(:,2*i);
    if Southeast_Asia_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Southeast_Asia_raw(:,2*i-1),Southeast_Asia_raw(:,2*i),x0,lb,ub);
        if fval < Southeast_Asia_fval(i);
            Southeast_Asia_fval(i) = fval; Southeast_Asia_exp(i) = exp; Southeast_Asia_mid(i) = mid;
            Southeast_Asia_fit(:,2*i-1) = fit;
        end
    end
end
end

Taiwan_raw = [Taiwan_c4_low_l_cum,Taiwan_c4_low_l_sort(:,2),...
    Taiwan_c4_high_l_cum,Taiwan_c4_high_l_sort(:,2),...
    Taiwan_c4_low_h_cum,Taiwan_c4_low_h_sort(:,2),...
    Taiwan_c4_high_h_cum,Taiwan_c4_high_h_sort(:,2),...
    Taiwan_c5_low_l_cum,Taiwan_c5_low_l_sort(:,2),...
    Taiwan_c5_high_l_cum,Taiwan_c5_high_l_sort(:,2),...
    Taiwan_c5_low_h_cum,Taiwan_c5_low_h_sort(:,2),...
    Taiwan_c5_high_h_cum,Taiwan_c5_high_h_sort(:,2),...
    Taiwan_c6_low_l_cum,Taiwan_c6_low_l_sort(:,2),...
    Taiwan_c6_high_l_cum,Taiwan_c6_high_l_sort(:,2),...
    Taiwan_c6_low_h_cum,Taiwan_c6_low_h_sort(:,2),...
    Taiwan_c6_high_h_cum,Taiwan_c6_high_h_sort(:,2),...
    Taiwan_c7_low_l_cum,Taiwan_c7_low_l_sort(:,2),...
    Taiwan_c7_high_l_cum,Taiwan_c7_high_l_sort(:,2),...
    Taiwan_c7_low_h_cum,Taiwan_c7_low_h_sort(:,2),...
    Taiwan_c7_high_h_cum,Taiwan_c7_high_h_sort(:,2),...
    Taiwan_c89_low_l_cum,Taiwan_c89_low_l_sort(:,2),...
    Taiwan_c89_high_l_cum,Taiwan_c89_high_l_sort(:,2),...
    Taiwan_c89_low_h_cum,Taiwan_c89_low_h_sort(:,2),...
    Taiwan_c89_high_h_cum,Taiwan_c89_high_h_sort(:,2)];

Taiwan_fval = zeros(1,size(Taiwan_raw,2)/2);
Taiwan_exp = Taiwan_fval; Taiwan_mid = Taiwan_fval;
Taiwan_fit = zeros(size(Taiwan_raw));
Taiwan_error = zeros(size(Taiwan_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Taiwan_raw,2)/2;
if Taiwan_raw(:,2*i-1) == 0;
else
    [Taiwan_fval(i) Taiwan_exp(i) Taiwan_mid(i) Taiwan_fit(:,2*i-1) Taiwan_error(:,i)] = curve_fitting(Taiwan_raw(:,2*i-1),Taiwan_raw(:,2*i),x0,lb,ub);
    Taiwan_fit(:,2*i) = Taiwan_raw(:,2*i);
    if Taiwan_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Taiwan_raw(:,2*i-1),Taiwan_raw(:,2*i),x0,lb,ub);
        if fval < Taiwan_fval(i);
            Taiwan_fval(i) = fval; Taiwan_exp(i) = exp; Taiwan_mid(i) = mid;
            Taiwan_fit(:,2*i-1) = fit;
        end
    end
end
end

Argentina_raw = [Argentina_c4_low_l_cum,Argentina_c4_low_l_sort(:,2),...
    Argentina_c4_high_l_cum,Argentina_c4_high_l_sort(:,2),...
    Argentina_c4_low_h_cum,Argentina_c4_low_h_sort(:,2),...
    Argentina_c4_high_h_cum,Argentina_c4_high_h_sort(:,2),...
    Argentina_c5_low_l_cum,Argentina_c5_low_l_sort(:,2),...
    Argentina_c5_high_l_cum,Argentina_c5_high_l_sort(:,2),...
    Argentina_c5_low_h_cum,Argentina_c5_low_h_sort(:,2),...
    Argentina_c5_high_h_cum,Argentina_c5_high_h_sort(:,2),...
    Argentina_c6_low_l_cum,Argentina_c6_low_l_sort(:,2),...
    Argentina_c6_high_l_cum,Argentina_c6_high_l_sort(:,2),...
    Argentina_c6_low_h_cum,Argentina_c6_low_h_sort(:,2),...
    Argentina_c6_high_h_cum,Argentina_c6_high_h_sort(:,2),...
    Argentina_c7_low_l_cum,Argentina_c7_low_l_sort(:,2),...
    Argentina_c7_high_l_cum,Argentina_c7_high_l_sort(:,2),...
    Argentina_c7_low_h_cum,Argentina_c7_low_h_sort(:,2),...
    Argentina_c7_high_h_cum,Argentina_c7_high_h_sort(:,2),...
    Argentina_c89_low_l_cum,Argentina_c89_low_l_sort(:,2),...
    Argentina_c89_high_l_cum,Argentina_c89_high_l_sort(:,2),...
    Argentina_c89_low_h_cum,Argentina_c89_low_h_sort(:,2),...
    Argentina_c89_high_h_cum,Argentina_c89_high_h_sort(:,2)];

Argentina_fval = zeros(1,size(Argentina_raw,2)/2);
Argentina_exp = Argentina_fval; Argentina_mid = Argentina_fval;
Argentina_fit = zeros(size(Argentina_raw));
Argentina_error = zeros(size(Argentina_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Argentina_raw,2)/2;
if Argentina_raw(:,2*i-1) == 0;
else
    [Argentina_fval(i) Argentina_exp(i) Argentina_mid(i) Argentina_fit(:,2*i-1) Argentina_error(:,i)] = curve_fitting(Argentina_raw(:,2*i-1),Argentina_raw(:,2*i),x0,lb,ub);
    Argentina_fit(:,2*i) = Argentina_raw(:,2*i);
    if Argentina_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Argentina_raw(:,2*i-1),Argentina_raw(:,2*i),x0,lb,ub);
        if fval < Argentina_fval(i);
            Argentina_fval(i) = fval; Argentina_exp(i) = exp; Argentina_mid(i) = mid;
            Argentina_fit(:,2*i-1) = fit;
        end
    end
end
end

Columbia_raw = [Columbia_c4_low_l_cum,Columbia_c4_low_l_sort(:,2),...
    Columbia_c4_high_l_cum,Columbia_c4_high_l_sort(:,2),...
    Columbia_c4_low_h_cum,Columbia_c4_low_h_sort(:,2),...
    Columbia_c4_high_h_cum,Columbia_c4_high_h_sort(:,2),...
    Columbia_c5_low_l_cum,Columbia_c5_low_l_sort(:,2),...
    Columbia_c5_high_l_cum,Columbia_c5_high_l_sort(:,2),...
    Columbia_c5_low_h_cum,Columbia_c5_low_h_sort(:,2),...
    Columbia_c5_high_h_cum,Columbia_c5_high_h_sort(:,2),...
    Columbia_c6_low_l_cum,Columbia_c6_low_l_sort(:,2),...
    Columbia_c6_high_l_cum,Columbia_c6_high_l_sort(:,2),...
    Columbia_c6_low_h_cum,Columbia_c6_low_h_sort(:,2),...
    Columbia_c6_high_h_cum,Columbia_c6_high_h_sort(:,2),...
    Columbia_c7_low_l_cum,Columbia_c7_low_l_sort(:,2),...
    Columbia_c7_high_l_cum,Columbia_c7_high_l_sort(:,2),...
    Columbia_c7_low_h_cum,Columbia_c7_low_h_sort(:,2),...
    Columbia_c7_high_h_cum,Columbia_c7_high_h_sort(:,2),...
    Columbia_c89_low_l_cum,Columbia_c89_low_l_sort(:,2),...
    Columbia_c89_high_l_cum,Columbia_c89_high_l_sort(:,2),...
    Columbia_c89_low_h_cum,Columbia_c89_low_h_sort(:,2),...
    Columbia_c89_high_h_cum,Columbia_c89_high_h_sort(:,2)];

Columbia_fval = zeros(1,size(Columbia_raw,2)/2);
Columbia_exp = Columbia_fval; Columbia_mid = Columbia_fval;
Columbia_fit = zeros(size(Columbia_raw));
Columbia_error = zeros(size(Columbia_raw,1),7);
x0 = [1,1];
lb = [exp_lb,0.001];
ub = [exp_ub,50];
for i = 1:size(Columbia_raw,2)/2;
if Columbia_raw(:,2*i-1) == 0;
else
    [Columbia_fval(i) Columbia_exp(i) Columbia_mid(i) Columbia_fit(:,2*i-1) Columbia_error(:,i)] = curve_fitting(Columbia_raw(:,2*i-1),Columbia_raw(:,2*i),x0,lb,ub);
    Columbia_fit(:,2*i) = Columbia_raw(:,2*i);
    if Columbia_fval(i) > 0.1;
        lb = [exp_lb_2,0.001];
        [fval exp mid fit] = curve_fitting(Columbia_raw(:,2*i-1),Columbia_raw(:,2*i),x0,lb,ub);
        if fval < Columbia_fval(i);
            Columbia_fval(i) = fval; Columbia_exp(i) = exp; Columbia_mid(i) = mid;
            Columbia_fit(:,2*i-1) = fit;
        end
    end
end
end


%% Output to GCAM files

% for RenewRsrcCurves files
owe1_usa_high = [CA_1_low_h_cum(end),CA_mid(3),CA_exp(3);
                 CT_1_low_h_cum(end),CT_mid(3),CT_exp(3);
                 DE_1_low_h_cum(end),DE_mid(3),DE_exp(3);
                 GA_1_low_h_cum(end),GA_mid(3),GA_exp(3);
                 HI_1_low_h_cum(end),HI_mid(3),HI_exp(3);
                 IL_1_low_h_cum(end),IL_mid(3),IL_exp(3);
                 IN_1_low_h_cum(end),IN_mid(3),IN_exp(3);
                 LA_1_low_h_cum(end),LA_mid(3),LA_exp(3);
                 ME_1_low_h_cum(end),ME_mid(3),ME_exp(3);
                 MA_1_low_h_cum(end),MA_mid(3),MA_exp(3);
                 MD_1_low_h_cum(end),MD_mid(3),MD_exp(3);
                 MI_1_low_h_cum(end),MI_mid(3),MI_exp(3);
                 MN_1_low_h_cum(end),MN_mid(3),MN_exp(3);
                 NH_1_low_h_cum(end),NH_mid(3),NH_exp(3);
                 NJ_1_low_h_cum(end),NJ_mid(3),NJ_exp(3);
                 NY_1_low_h_cum(end),NY_mid(3),NY_exp(3);
                 NC_1_low_h_cum(end),NC_mid(3),NC_exp(3);
                 OH_1_low_h_cum(end),OH_mid(3),OH_exp(3);
                 OR_1_low_h_cum(end),OR_mid(3),OR_exp(3);
                 PA_1_low_h_cum(end),PA_mid(3),PA_exp(3);
                 RI_1_low_h_cum(end),RI_mid(3),RI_exp(3);
                 SC_1_low_h_cum(end),SC_mid(3),SC_exp(3);
                 TX_1_low_h_cum(end),TX_mid(3),TX_exp(3);
                 VA_1_low_h_cum(end),VA_mid(3),VA_exp(3);
                 WA_1_low_h_cum(end),WA_mid(3),WA_exp(3);
                 WI_1_low_h_cum(end),WI_mid(3),WI_exp(3)];
xlswrite('L210.RenewRsrcCurves_owe1_smooth_usa_high.csv', owe1_usa_high,'D6:F31')

owe2_usa_high = [CA_2_low_h_cum(end),CA_mid(7),CA_exp(7);
                 CT_2_low_h_cum(end),CT_mid(7),CT_exp(7);
                 DE_2_low_h_cum(end),DE_mid(7),DE_exp(7);
                 GA_2_low_h_cum(end),GA_mid(7),GA_exp(7);
                 HI_2_low_h_cum(end),HI_mid(7),HI_exp(7);
                 IL_2_low_h_cum(end),IL_mid(7),IL_exp(7);
                 IN_2_low_h_cum(end),IN_mid(7),IN_exp(7);
                 LA_2_low_h_cum(end),LA_mid(7),LA_exp(7);
                 ME_2_low_h_cum(end),ME_mid(7),ME_exp(7);
                 MA_2_low_h_cum(end),MA_mid(7),MA_exp(7);
                 MD_2_low_h_cum(end),MD_mid(7),MD_exp(7);
                 MI_2_low_h_cum(end),MI_mid(7),MI_exp(7);
                 MN_2_low_h_cum(end),MN_mid(7),MN_exp(7);
                 NH_2_low_h_cum(end),NH_mid(7),NH_exp(7);
                 NJ_2_low_h_cum(end),NJ_mid(7),NJ_exp(7);
                 NY_2_low_h_cum(end),NY_mid(7),NY_exp(7);
                 NC_2_low_h_cum(end),NC_mid(7),NC_exp(7);
                 OH_2_low_h_cum(end),OH_mid(7),OH_exp(7);
                 OR_2_low_h_cum(end),OR_mid(7),OR_exp(7);
                 PA_2_low_h_cum(end),PA_mid(7),PA_exp(7);
                 RI_2_low_h_cum(end),RI_mid(7),RI_exp(7);
                 SC_2_low_h_cum(end),SC_mid(7),SC_exp(7);
                 TX_2_low_h_cum(end),TX_mid(7),TX_exp(7);
                 VA_2_low_h_cum(end),VA_mid(7),VA_exp(7);
                 WA_2_low_h_cum(end),WA_mid(7),WA_exp(7);
                 WI_2_low_h_cum(end),WI_mid(7),WI_exp(7)];
xlswrite('L210.RenewRsrcCurves_owe2_smooth_usa_high.csv', owe2_usa_high,'D6:F31')

owe3_usa_high = [CA_3_low_h_cum(end),CA_mid(11),CA_exp(11);
                 CT_3_low_h_cum(end),CT_mid(11),CT_exp(11);
                 DE_3_low_h_cum(end),DE_mid(11),DE_exp(11);
                 GA_3_low_h_cum(end),GA_mid(11),GA_exp(11);
                 HI_3_low_h_cum(end),HI_mid(11),HI_exp(11);
                 IL_3_low_h_cum(end),IL_mid(11),IL_exp(11);
                 IN_3_low_h_cum(end),IN_mid(11),IN_exp(11);
                 LA_3_low_h_cum(end),LA_mid(11),LA_exp(11);
                 ME_3_low_h_cum(end),ME_mid(11),ME_exp(11);
                 MA_3_low_h_cum(end),MA_mid(11),MA_exp(11);
                 MD_3_low_h_cum(end),MD_mid(11),MD_exp(11);
                 MI_3_low_h_cum(end),MI_mid(11),MI_exp(11);
                 MN_3_low_h_cum(end),MN_mid(11),MN_exp(11);
                 NH_3_low_h_cum(end),NH_mid(11),NH_exp(11);
                 NJ_3_low_h_cum(end),NJ_mid(11),NJ_exp(11);
                 NY_3_low_h_cum(end),NY_mid(11),NY_exp(11);
                 NC_3_low_h_cum(end),NC_mid(11),NC_exp(11);
                 OH_3_low_h_cum(end),OH_mid(11),OH_exp(11);
                 OR_3_low_h_cum(end),OR_mid(11),OR_exp(11);
                 PA_3_low_h_cum(end),PA_mid(11),PA_exp(11);
                 RI_3_low_h_cum(end),RI_mid(11),RI_exp(11);
                 SC_3_low_h_cum(end),SC_mid(11),SC_exp(11);
                 TX_3_low_h_cum(end),TX_mid(11),TX_exp(11);
                 VA_3_low_h_cum(end),VA_mid(11),VA_exp(11);
                 WA_3_low_h_cum(end),WA_mid(11),WA_exp(11);
                 WI_3_low_h_cum(end),WI_mid(11),WI_exp(11)];
owe3_usa_high_mod = owe3_usa_high(owe3_usa_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe3_smooth_usa_high.csv', owe3_usa_high,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe3_smooth_usa_high_mod.csv', owe3_usa_high_mod,'D6:F31')

owe4_usa_high = [CA_4_low_h_cum(end),CA_mid(15),CA_exp(15);
                 CT_4_low_h_cum(end),CT_mid(15),CT_exp(15);
                 DE_4_low_h_cum(end),DE_mid(15),DE_exp(15);
                 GA_4_low_h_cum(end),GA_mid(15),GA_exp(15);
                 HI_4_low_h_cum(end),HI_mid(15),HI_exp(15);
                 IL_4_low_h_cum(end),IL_mid(15),IL_exp(15);
                 IN_4_low_h_cum(end),IN_mid(15),IN_exp(15);
                 LA_4_low_h_cum(end),LA_mid(15),LA_exp(15);
                 ME_4_low_h_cum(end),ME_mid(15),ME_exp(15);
                 MA_4_low_h_cum(end),MA_mid(15),MA_exp(15);
                 MD_4_low_h_cum(end),MD_mid(15),MD_exp(15);
                 MI_4_low_h_cum(end),MI_mid(15),MI_exp(15);
                 MN_4_low_h_cum(end),MN_mid(15),MN_exp(15);
                 NH_4_low_h_cum(end),NH_mid(15),NH_exp(15);
                 NJ_4_low_h_cum(end),NJ_mid(15),NJ_exp(15);
                 NY_4_low_h_cum(end),NY_mid(15),NY_exp(15);
                 NC_4_low_h_cum(end),NC_mid(15),NC_exp(15);
                 OH_4_low_h_cum(end),OH_mid(15),OH_exp(15);
                 OR_4_low_h_cum(end),OR_mid(15),OR_exp(15);
                 PA_4_low_h_cum(end),PA_mid(15),PA_exp(15);
                 RI_4_low_h_cum(end),RI_mid(15),RI_exp(15);
                 SC_4_low_h_cum(end),SC_mid(15),SC_exp(15);
                 TX_4_low_h_cum(end),TX_mid(15),TX_exp(15);
                 VA_4_low_h_cum(end),VA_mid(15),VA_exp(15);
                 WA_4_low_h_cum(end),WA_mid(15),WA_exp(15);
                 WI_4_low_h_cum(end),WI_mid(15),WI_exp(15)];
owe4_usa_high_mod = owe4_usa_high(owe4_usa_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe4_smooth_usa_high.csv', owe4_usa_high,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe4_smooth_usa_high_mod.csv', owe4_usa_high_mod,'D6:F31')

owe5_usa_high = [CA_5_low_h_cum(end),CA_mid(19),CA_exp(19);
                 CT_5_low_h_cum(end),CT_mid(19),CT_exp(19);
                 DE_5_low_h_cum(end),DE_mid(19),DE_exp(19);
                 GA_5_low_h_cum(end),GA_mid(19),GA_exp(19);
                 HI_5_low_h_cum(end),HI_mid(19),HI_exp(19);
                 IL_5_low_h_cum(end),IL_mid(19),IL_exp(19);
                 IN_5_low_h_cum(end),IN_mid(19),IN_exp(19);
                 LA_5_low_h_cum(end),LA_mid(19),LA_exp(19);
                 ME_5_low_h_cum(end),ME_mid(19),ME_exp(19);
                 MA_5_low_h_cum(end),MA_mid(19),MA_exp(19);
                 MD_5_low_h_cum(end),MD_mid(19),MD_exp(19);
                 MI_5_low_h_cum(end),MI_mid(19),MI_exp(19);
                 MN_5_low_h_cum(end),MN_mid(19),MN_exp(19);
                 NH_5_low_h_cum(end),NH_mid(19),NH_exp(19);
                 NJ_5_low_h_cum(end),NJ_mid(19),NJ_exp(19);
                 NY_5_low_h_cum(end),NY_mid(19),NY_exp(19);
                 NC_5_low_h_cum(end),NC_mid(19),NC_exp(19);
                 OH_5_low_h_cum(end),OH_mid(19),OH_exp(19);
                 OR_5_low_h_cum(end),OR_mid(19),OR_exp(19);
                 PA_5_low_h_cum(end),PA_mid(19),PA_exp(19);
                 RI_5_low_h_cum(end),RI_mid(19),RI_exp(19);
                 SC_5_low_h_cum(end),SC_mid(19),SC_exp(19);
                 TX_5_low_h_cum(end),TX_mid(19),TX_exp(19);
                 VA_5_low_h_cum(end),VA_mid(19),VA_exp(19);
                 WA_5_low_h_cum(end),WA_mid(19),WA_exp(19);
                 WI_5_low_h_cum(end),WI_mid(19),WI_exp(19)];
owe5_usa_high_mod = owe5_usa_high(owe5_usa_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe5_smooth_usa_high.csv', owe5_usa_high,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe5_smooth_usa_high_mod.csv', owe5_usa_high_mod,'D6:F31')

owe6_usa_high = [CA_6_low_h_cum(end),CA_mid(23),CA_exp(23);
                 CT_6_low_h_cum(end),CT_mid(23),CT_exp(23);
                 DE_6_low_h_cum(end),DE_mid(23),DE_exp(23);
                 GA_6_low_h_cum(end),GA_mid(23),GA_exp(23);
                 HI_6_low_h_cum(end),HI_mid(23),HI_exp(23);
                 IL_6_low_h_cum(end),IL_mid(23),IL_exp(23);
                 IN_6_low_h_cum(end),IN_mid(23),IN_exp(23);
                 LA_6_low_h_cum(end),LA_mid(23),LA_exp(23);
                 ME_6_low_h_cum(end),ME_mid(23),ME_exp(23);
                 MA_6_low_h_cum(end),MA_mid(23),MA_exp(23);
                 MD_6_low_h_cum(end),MD_mid(23),MD_exp(23);
                 MI_6_low_h_cum(end),MI_mid(23),MI_exp(23);
                 MN_6_low_h_cum(end),MN_mid(23),MN_exp(23);
                 NH_6_low_h_cum(end),NH_mid(23),NH_exp(23);
                 NJ_6_low_h_cum(end),NJ_mid(23),NJ_exp(23);
                 NY_6_low_h_cum(end),NY_mid(23),NY_exp(23);
                 NC_6_low_h_cum(end),NC_mid(23),NC_exp(23);
                 OH_6_low_h_cum(end),OH_mid(23),OH_exp(23);
                 OR_6_low_h_cum(end),OR_mid(23),OR_exp(23);
                 PA_6_low_h_cum(end),PA_mid(23),PA_exp(23);
                 RI_6_low_h_cum(end),RI_mid(23),RI_exp(23);
                 SC_6_low_h_cum(end),SC_mid(23),SC_exp(23);
                 TX_6_low_h_cum(end),TX_mid(23),TX_exp(23);
                 VA_6_low_h_cum(end),VA_mid(23),VA_exp(23);
                 WA_6_low_h_cum(end),WA_mid(23),WA_exp(23);
                 WI_6_low_h_cum(end),WI_mid(23),WI_exp(23)];
owe6_usa_high_mod = owe6_usa_high(owe6_usa_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe6_smooth_usa_high.csv', owe6_usa_high,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe6_smooth_usa_high_mod.csv', owe6_usa_high_mod,'D6:F31')

owe7_usa_high = [CA_7_low_h_cum(end),CA_mid(27),CA_exp(27);
                 CT_7_low_h_cum(end),CT_mid(27),CT_exp(27);
                 DE_7_low_h_cum(end),DE_mid(27),DE_exp(27);
                 GA_7_low_h_cum(end),GA_mid(27),GA_exp(27);
                 HI_7_low_h_cum(end),HI_mid(27),HI_exp(27);
                 IL_7_low_h_cum(end),IL_mid(27),IL_exp(27);
                 IN_7_low_h_cum(end),IN_mid(27),IN_exp(27);
                 LA_7_low_h_cum(end),LA_mid(27),LA_exp(27);
                 ME_7_low_h_cum(end),ME_mid(27),ME_exp(27);
                 MA_7_low_h_cum(end),MA_mid(27),MA_exp(27);
                 MD_7_low_h_cum(end),MD_mid(27),MD_exp(27);
                 MI_7_low_h_cum(end),MI_mid(27),MI_exp(27);
                 MN_7_low_h_cum(end),MN_mid(27),MN_exp(27);
                 NH_7_low_h_cum(end),NH_mid(27),NH_exp(27);
                 NJ_7_low_h_cum(end),NJ_mid(27),NJ_exp(27);
                 NY_7_low_h_cum(end),NY_mid(27),NY_exp(27);
                 NC_7_low_h_cum(end),NC_mid(27),NC_exp(27);
                 OH_7_low_h_cum(end),OH_mid(27),OH_exp(27);
                 OR_7_low_h_cum(end),OR_mid(27),OR_exp(27);
                 PA_7_low_h_cum(end),PA_mid(27),PA_exp(27);
                 RI_7_low_h_cum(end),RI_mid(27),RI_exp(27);
                 SC_7_low_h_cum(end),SC_mid(27),SC_exp(27);
                 TX_7_low_h_cum(end),TX_mid(27),TX_exp(27);
                 VA_7_low_h_cum(end),VA_mid(27),VA_exp(27);
                 WA_7_low_h_cum(end),WA_mid(27),WA_exp(27);
                 WI_7_low_h_cum(end),WI_mid(27),WI_exp(27)];
owe7_usa_high_mod = owe7_usa_high(owe7_usa_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe7_smooth_usa_high.csv', owe7_usa_high,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe7_smooth_usa_high_mod.csv', owe7_usa_high_mod,'D6:F31')
             
owe1_usa_low = [CA_1_high_l_cum(end),CA_mid(2),CA_exp(2);
                 CT_1_high_l_cum(end),CT_mid(2),CT_exp(2);
                 DE_1_high_l_cum(end),DE_mid(2),DE_exp(2);
                 GA_1_high_l_cum(end),GA_mid(2),GA_exp(2);
                 HI_1_high_l_cum(end),HI_mid(2),HI_exp(2);
                 IL_1_high_l_cum(end),IL_mid(2),IL_exp(2);
                 IN_1_high_l_cum(end),IN_mid(2),IN_exp(2);
                 LA_1_high_l_cum(end),LA_mid(2),LA_exp(2);
                 ME_1_high_l_cum(end),ME_mid(2),ME_exp(2);
                 MA_1_high_l_cum(end),MA_mid(2),MA_exp(2);
                 MD_1_high_l_cum(end),MD_mid(2),MD_exp(2);
                 MI_1_high_l_cum(end),MI_mid(2),MI_exp(2);
                 MN_1_high_l_cum(end),MN_mid(2),MN_exp(2);
                 NH_1_high_l_cum(end),NH_mid(2),NH_exp(2);
                 NJ_1_high_l_cum(end),NJ_mid(2),NJ_exp(2);
                 NY_1_high_l_cum(end),NY_mid(2),NY_exp(2);
                 NC_1_high_l_cum(end),NC_mid(2),NC_exp(2);
                 OH_1_high_l_cum(end),OH_mid(2),OH_exp(2);
                 OR_1_high_l_cum(end),OR_mid(2),OR_exp(2);
                 PA_1_high_l_cum(end),PA_mid(2),PA_exp(2);
                 RI_1_high_l_cum(end),RI_mid(2),RI_exp(2);
                 SC_1_high_l_cum(end),SC_mid(2),SC_exp(2);
                 TX_1_high_l_cum(end),TX_mid(2),TX_exp(2);
                 VA_1_high_l_cum(end),VA_mid(2),VA_exp(2);
                 WA_1_high_l_cum(end),WA_mid(2),WA_exp(2);
                 WI_1_high_l_cum(end),WI_mid(2),WI_exp(2)];
xlswrite('L210.RenewRsrcCurves_owe1_smooth_usa_low.csv', owe1_usa_low,'D6:F31')

owe2_usa_low = [CA_2_high_l_cum(end),CA_mid(6),CA_exp(6);
                 CT_2_high_l_cum(end),CT_mid(6),CT_exp(6);
                 DE_2_high_l_cum(end),DE_mid(6),DE_exp(6);
                 GA_2_high_l_cum(end),GA_mid(6),GA_exp(6);
                 HI_2_high_l_cum(end),HI_mid(6),HI_exp(6);
                 IL_2_high_l_cum(end),IL_mid(6),IL_exp(6);
                 IN_2_high_l_cum(end),IN_mid(6),IN_exp(6);
                 LA_2_high_l_cum(end),LA_mid(6),LA_exp(6);
                 ME_2_high_l_cum(end),ME_mid(6),ME_exp(6);
                 MA_2_high_l_cum(end),MA_mid(6),MA_exp(6);
                 MD_2_high_l_cum(end),MD_mid(6),MD_exp(6);
                 MI_2_high_l_cum(end),MI_mid(6),MI_exp(6);
                 MN_2_high_l_cum(end),MN_mid(6),MN_exp(6);
                 NH_2_high_l_cum(end),NH_mid(6),NH_exp(6);
                 NJ_2_high_l_cum(end),NJ_mid(6),NJ_exp(6);
                 NY_2_high_l_cum(end),NY_mid(6),NY_exp(6);
                 NC_2_high_l_cum(end),NC_mid(6),NC_exp(6);
                 OH_2_high_l_cum(end),OH_mid(6),OH_exp(6);
                 OR_2_high_l_cum(end),OR_mid(6),OR_exp(6);
                 PA_2_high_l_cum(end),PA_mid(6),PA_exp(6);
                 RI_2_high_l_cum(end),RI_mid(6),RI_exp(6);
                 SC_2_high_l_cum(end),SC_mid(6),SC_exp(6);
                 TX_2_high_l_cum(end),TX_mid(6),TX_exp(6);
                 VA_2_high_l_cum(end),VA_mid(6),VA_exp(6);
                 WA_2_high_l_cum(end),WA_mid(6),WA_exp(6);
                 WI_2_high_l_cum(end),WI_mid(6),WI_exp(6)];
xlswrite('L210.RenewRsrcCurves_owe2_smooth_usa_low.csv', owe2_usa_low,'D6:F31')
             
owe3_usa_low = [CA_3_high_l_cum(end),CA_mid(10),CA_exp(10);
                 CT_3_high_l_cum(end),CT_mid(10),CT_exp(10);
                 DE_3_high_l_cum(end),DE_mid(10),DE_exp(10);
                 GA_3_high_l_cum(end),GA_mid(10),GA_exp(10);
                 HI_3_high_l_cum(end),HI_mid(10),HI_exp(10);
                 IL_3_high_l_cum(end),IL_mid(10),IL_exp(10);
                 IN_3_high_l_cum(end),IN_mid(10),IN_exp(10);
                 LA_3_high_l_cum(end),LA_mid(10),LA_exp(10);
                 ME_3_high_l_cum(end),ME_mid(10),ME_exp(10);
                 MA_3_high_l_cum(end),MA_mid(10),MA_exp(10);
                 MD_3_high_l_cum(end),MD_mid(10),MD_exp(10);
                 MI_3_high_l_cum(end),MI_mid(10),MI_exp(10);
                 MN_3_high_l_cum(end),MN_mid(10),MN_exp(10);
                 NH_3_high_l_cum(end),NH_mid(10),NH_exp(10);
                 NJ_3_high_l_cum(end),NJ_mid(10),NJ_exp(10);
                 NY_3_high_l_cum(end),NY_mid(10),NY_exp(10);
                 NC_3_high_l_cum(end),NC_mid(10),NC_exp(10);
                 OH_3_high_l_cum(end),OH_mid(10),OH_exp(10);
                 OR_3_high_l_cum(end),OR_mid(10),OR_exp(10);
                 PA_3_high_l_cum(end),PA_mid(10),PA_exp(10);
                 RI_3_high_l_cum(end),RI_mid(10),RI_exp(10);
                 SC_3_high_l_cum(end),SC_mid(10),SC_exp(10);
                 TX_3_high_l_cum(end),TX_mid(10),TX_exp(10);
                 VA_3_high_l_cum(end),VA_mid(10),VA_exp(10);
                 WA_3_high_l_cum(end),WA_mid(10),WA_exp(10);
                 WI_3_high_l_cum(end),WI_mid(10),WI_exp(10)];
owe3_usa_low_mod = owe3_usa_low(owe3_usa_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe3_smooth_usa_low.csv', owe3_usa_low,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe3_smooth_usa_low_mod.csv', owe3_usa_low_mod,'D6:F31')
             
owe4_usa_low = [CA_4_high_l_cum(end),CA_mid(14),CA_exp(14);
                 CT_4_high_l_cum(end),CT_mid(14),CT_exp(14);
                 DE_4_high_l_cum(end),DE_mid(14),DE_exp(14);
                 GA_4_high_l_cum(end),GA_mid(14),GA_exp(14);
                 HI_4_high_l_cum(end),HI_mid(14),HI_exp(14);
                 IL_4_high_l_cum(end),IL_mid(14),IL_exp(14);
                 IN_4_high_l_cum(end),IN_mid(14),IN_exp(14);
                 LA_4_high_l_cum(end),LA_mid(14),LA_exp(14);
                 ME_4_high_l_cum(end),ME_mid(14),ME_exp(14);
                 MA_4_high_l_cum(end),MA_mid(14),MA_exp(14);
                 MD_4_high_l_cum(end),MD_mid(14),MD_exp(14);
                 MI_4_high_l_cum(end),MI_mid(14),MI_exp(14);
                 MN_4_high_l_cum(end),MN_mid(14),MN_exp(14);
                 NH_4_high_l_cum(end),NH_mid(14),NH_exp(14);
                 NJ_4_high_l_cum(end),NJ_mid(14),NJ_exp(14);
                 NY_4_high_l_cum(end),NY_mid(14),NY_exp(14);
                 NC_4_high_l_cum(end),NC_mid(14),NC_exp(14);
                 OH_4_high_l_cum(end),OH_mid(14),OH_exp(14);
                 OR_4_high_l_cum(end),OR_mid(14),OR_exp(14);
                 PA_4_high_l_cum(end),PA_mid(14),PA_exp(14);
                 RI_4_high_l_cum(end),RI_mid(14),RI_exp(14);
                 SC_4_high_l_cum(end),SC_mid(14),SC_exp(14);
                 TX_4_high_l_cum(end),TX_mid(14),TX_exp(14);
                 VA_4_high_l_cum(end),VA_mid(14),VA_exp(14);
                 WA_4_high_l_cum(end),WA_mid(14),WA_exp(14);
                 WI_4_high_l_cum(end),WI_mid(14),WI_exp(14)];
owe4_usa_low_mod = owe4_usa_low(owe4_usa_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe4_smooth_usa_low.csv', owe4_usa_low,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe4_smooth_usa_low_mod.csv', owe4_usa_low_mod,'D6:F31')

owe5_usa_low = [CA_5_high_l_cum(end),CA_mid(18),CA_exp(18);
                 CT_5_high_l_cum(end),CT_mid(18),CT_exp(18);
                 DE_5_high_l_cum(end),DE_mid(18),DE_exp(18);
                 GA_5_high_l_cum(end),GA_mid(18),GA_exp(18);
                 HI_5_high_l_cum(end),HI_mid(18),HI_exp(18);
                 IL_5_high_l_cum(end),IL_mid(18),IL_exp(18);
                 IN_5_high_l_cum(end),IN_mid(18),IN_exp(18);
                 LA_5_high_l_cum(end),LA_mid(18),LA_exp(18);
                 ME_5_high_l_cum(end),ME_mid(18),ME_exp(18);
                 MA_5_high_l_cum(end),MA_mid(18),MA_exp(18);
                 MD_5_high_l_cum(end),MD_mid(18),MD_exp(18);
                 MI_5_high_l_cum(end),MI_mid(18),MI_exp(18);
                 MN_5_high_l_cum(end),MN_mid(18),MN_exp(18);
                 NH_5_high_l_cum(end),NH_mid(18),NH_exp(18);
                 NJ_5_high_l_cum(end),NJ_mid(18),NJ_exp(18);
                 NY_5_high_l_cum(end),NY_mid(18),NY_exp(18);
                 NC_5_high_l_cum(end),NC_mid(18),NC_exp(18);
                 OH_5_high_l_cum(end),OH_mid(18),OH_exp(18);
                 OR_5_high_l_cum(end),OR_mid(18),OR_exp(18);
                 PA_5_high_l_cum(end),PA_mid(18),PA_exp(18);
                 RI_5_high_l_cum(end),RI_mid(18),RI_exp(18);
                 SC_5_high_l_cum(end),SC_mid(18),SC_exp(18);
                 TX_5_high_l_cum(end),TX_mid(18),TX_exp(18);
                 VA_5_high_l_cum(end),VA_mid(18),VA_exp(18);
                 WA_5_high_l_cum(end),WA_mid(18),WA_exp(18);
                 WI_5_high_l_cum(end),WI_mid(18),WI_exp(18)];
owe5_usa_low_mod = owe5_usa_low(owe5_usa_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe5_smooth_usa_low.csv', owe5_usa_low,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe5_smooth_usa_low_mod.csv', owe5_usa_low_mod,'D6:F31')
             
owe6_usa_low = [CA_6_high_l_cum(end),CA_mid(22),CA_exp(22);
                 CT_6_high_l_cum(end),CT_mid(22),CT_exp(22);
                 DE_6_high_l_cum(end),DE_mid(22),DE_exp(22);
                 GA_6_high_l_cum(end),GA_mid(22),GA_exp(22);
                 HI_6_high_l_cum(end),HI_mid(22),HI_exp(22);
                 IL_6_high_l_cum(end),IL_mid(22),IL_exp(22);
                 IN_6_high_l_cum(end),IN_mid(22),IN_exp(22);
                 LA_6_high_l_cum(end),LA_mid(22),LA_exp(22);
                 ME_6_high_l_cum(end),ME_mid(22),ME_exp(22);
                 MA_6_high_l_cum(end),MA_mid(22),MA_exp(22);
                 MD_6_high_l_cum(end),MD_mid(22),MD_exp(22);
                 MI_6_high_l_cum(end),MI_mid(22),MI_exp(22);
                 MN_6_high_l_cum(end),MN_mid(22),MN_exp(22);
                 NH_6_high_l_cum(end),NH_mid(22),NH_exp(22);
                 NJ_6_high_l_cum(end),NJ_mid(22),NJ_exp(22);
                 NY_6_high_l_cum(end),NY_mid(22),NY_exp(22);
                 NC_6_high_l_cum(end),NC_mid(22),NC_exp(22);
                 OH_6_high_l_cum(end),OH_mid(22),OH_exp(22);
                 OR_6_high_l_cum(end),OR_mid(22),OR_exp(22);
                 PA_6_high_l_cum(end),PA_mid(22),PA_exp(22);
                 RI_6_high_l_cum(end),RI_mid(22),RI_exp(22);
                 SC_6_high_l_cum(end),SC_mid(22),SC_exp(22);
                 TX_6_high_l_cum(end),TX_mid(22),TX_exp(22);
                 VA_6_high_l_cum(end),VA_mid(22),VA_exp(22);
                 WA_6_high_l_cum(end),WA_mid(22),WA_exp(22);
                 WI_6_high_l_cum(end),WI_mid(22),WI_exp(22)];
owe6_usa_low_mod = owe6_usa_low(owe6_usa_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe6_smooth_usa_low.csv', owe6_usa_low,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe6_smooth_usa_low_mod.csv', owe6_usa_low_mod,'D6:F31')
             
owe7_usa_low = [CA_7_high_l_cum(end),CA_mid(26),CA_exp(26);
                 CT_7_high_l_cum(end),CT_mid(26),CT_exp(26);
                 DE_7_high_l_cum(end),DE_mid(26),DE_exp(26);
                 GA_7_high_l_cum(end),GA_mid(26),GA_exp(26);
                 HI_7_high_l_cum(end),HI_mid(26),HI_exp(26);
                 IL_7_high_l_cum(end),IL_mid(26),IL_exp(26);
                 IN_7_high_l_cum(end),IN_mid(26),IN_exp(26);
                 LA_7_high_l_cum(end),LA_mid(26),LA_exp(26);
                 ME_7_high_l_cum(end),ME_mid(26),ME_exp(26);
                 MA_7_high_l_cum(end),MA_mid(26),MA_exp(26);
                 MD_7_high_l_cum(end),MD_mid(26),MD_exp(26);
                 MI_7_high_l_cum(end),MI_mid(26),MI_exp(26);
                 MN_7_high_l_cum(end),MN_mid(26),MN_exp(26);
                 NH_7_high_l_cum(end),NH_mid(26),NH_exp(26);
                 NJ_7_high_l_cum(end),NJ_mid(26),NJ_exp(26);
                 NY_7_high_l_cum(end),NY_mid(26),NY_exp(26);
                 NC_7_high_l_cum(end),NC_mid(26),NC_exp(26);
                 OH_7_high_l_cum(end),OH_mid(26),OH_exp(26);
                 OR_7_high_l_cum(end),OR_mid(26),OR_exp(26);
                 PA_7_high_l_cum(end),PA_mid(26),PA_exp(26);
                 RI_7_high_l_cum(end),RI_mid(26),RI_exp(26);
                 SC_7_high_l_cum(end),SC_mid(26),SC_exp(26);
                 TX_7_high_l_cum(end),TX_mid(26),TX_exp(26);
                 VA_7_high_l_cum(end),VA_mid(26),VA_exp(26);
                 WA_7_high_l_cum(end),WA_mid(26),WA_exp(26);
                 WI_7_high_l_cum(end),WI_mid(26),WI_exp(26)];             
owe7_usa_low_mod = owe7_usa_low(owe7_usa_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owe7_smooth_usa_low.csv', owe7_usa_low,'D6:F31')
xlswrite('L210.RenewRsrcCurves_owe7_smooth_usa_low_mod.csv', owe7_usa_low_mod,'D6:F31')


load onshore_cap
load onshore_cap_storage
load onshore_cap_adv
storage = (onshore_cap_storage(:,1)-onshore_cap(:,1))*to_gcam_dollars;
tech_change = zeros(22,1);
tech_change_storage = zeros(22,1);
storage_change = zeros(22,1);
tech_change_adv = zeros(22,1);
for i = 6:22;
    storage_change(i) = storage(i)/storage(5);
    tech_change(i) = onshore_cap(i,1)/onshore_cap(5,1);
    tech_change_storage(i) = onshore_cap_storage(i,1)/onshore_cap_storage(5,1);
    tech_change_adv(i) = onshore_cap_adv(i,1)/onshore_cap_adv(5,1);
end

% for the GlobalIntTechCapital files
cap1l_over_time = zeros(22,3);
cap1l_over_time(1:5,1) = total_capital_low(1,1);
cap1h_over_time = zeros(22,3);
cap1h_over_time(1:5,1) = total_capital_high(1,1);
cap1l_over_time(:,2) = 0.13;
cap1h_over_time(:,2) = 0.13;
cap1l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(1)));
cap1h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(1)));
cap1l_adv_over_time = cap1l_over_time;
cap1h_adv_over_time = cap1h_over_time;
for i = 6:22;
    cap1l_over_time(i,1) = cap1l_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    cap1h_over_time(i,1) = cap1h_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    cap1l_adv_over_time(i,1) = cap1l_adv_over_time(i-1,1)*0.75;
    cap1h_adv_over_time(i,1) = cap1h_adv_over_time(i-1,1)*0.75;
%     if i > 12; 
%         cap1l_over_time(i,1) = cap1l_over_time(i-1,1)*0.97; 
%         cap1h_over_time(i,1) = cap1h_over_time(i-1,1)*0.97; 
%     end
    if i > 8;
        cap1l_adv_over_time(i,1) = cap1l_adv_over_time(i-1,1)*0.95;
        cap1h_adv_over_time(i,1) = cap1h_adv_over_time(i-1,1)*0.95;
    end
end
cap1l_over_time(:,1) = cap1l_over_time(:,1)/to_gcam_dollars;
cap1h_over_time(:,1) = cap1h_over_time(:,1)/to_gcam_dollars;
cap1l_adv_over_time(:,1) = cap1l_adv_over_time(:,1)/to_gcam_dollars;
cap1h_adv_over_time(:,1) = cap1h_adv_over_time(:,1)/to_gcam_dollars;
xlswrite('L223.GlobalIntTechCapital_elec_owe1_low.csv',cap1l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe1_high.csv',cap1h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owe1_low_adv.csv',cap1l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe1_high_adv.csv',cap1h_adv_over_time,'F6:H27');      

cap2l_over_time = cap1l_over_time;
cap2h_over_time = cap1h_over_time;
cap2l_adv_over_time = cap1l_adv_over_time;
cap2h_adv_over_time = cap1h_adv_over_time;
cap2l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(2)));
cap2h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(2)));
cap2l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(2)));
cap2h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(2)));
xlswrite('L223.GlobalIntTechCapital_elec_owe2_low.csv',cap2l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe2_high.csv',cap2h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owe2_low_adv.csv',cap2l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe2_high_adv.csv',cap2h_adv_over_time,'F6:H27');      

cap3l_over_time = cap1l_over_time;
cap3h_over_time = cap1h_over_time;
cap3l_adv_over_time = cap1l_adv_over_time;
cap3h_adv_over_time = cap1h_adv_over_time;
cap3l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(3)));
cap3h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(3)));
cap3l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(3)));
cap3h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(3)));
xlswrite('L223.GlobalIntTechCapital_elec_owe3_low.csv',cap3l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe3_high.csv',cap3h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owe3_low_adv.csv',cap3l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe3_high_adv.csv',cap3h_adv_over_time,'F6:H27');      

cap4l_over_time = cap1l_over_time;
cap4h_over_time = cap1h_over_time;
cap4l_adv_over_time = cap1l_adv_over_time;
cap4h_adv_over_time = cap1h_adv_over_time;
cap4l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(4)));
cap4h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(4)));
cap4l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(4)));
cap4h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(4)));
xlswrite('L223.GlobalIntTechCapital_elec_owe4_low.csv',cap4l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe4_high.csv',cap4h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owe4_low_adv.csv',cap4l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe4_high_adv.csv',cap4h_adv_over_time,'F6:H27');      

cap5l_over_time = cap1l_over_time;
cap5h_over_time = cap1h_over_time;
cap5l_adv_over_time = cap1l_adv_over_time;
cap5h_adv_over_time = cap1h_adv_over_time;
cap5l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(5)));
cap5h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(5)));
cap5l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(5)));
cap5h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(5)));
xlswrite('L223.GlobalIntTechCapital_elec_owe5_low.csv',cap5l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe5_high.csv',cap5h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owe5_low_adv.csv',cap5l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe5_high_adv.csv',cap5h_adv_over_time,'F6:H27');      

cap6l_over_time = cap1l_over_time;
cap6h_over_time = cap1h_over_time;
cap6l_adv_over_time = cap1l_adv_over_time;
cap6h_adv_over_time = cap1h_adv_over_time;
cap6l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(6)));
cap6h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(6)));
cap6l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(6)));
cap6h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(6)));
xlswrite('L223.GlobalIntTechCapital_elec_owe6_low.csv',cap6l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe6_high.csv',cap6h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owe6_low_adv.csv',cap6l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe6_high_adv.csv',cap6h_adv_over_time,'F6:H27');      

cap7l_over_time = cap1l_over_time;
cap7h_over_time = cap1h_over_time;
cap7l_adv_over_time = cap1l_adv_over_time;
cap7h_adv_over_time = cap1h_adv_over_time;
cap7l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(7)));
cap7h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(7)));
cap7l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(7)));
cap7h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(7)));
xlswrite('L223.GlobalIntTechCapital_elec_owe7_low.csv',cap7l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe7_high.csv',cap7h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owe7_low_adv.csv',cap7l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owe7_high_adv.csv',cap7h_adv_over_time,'F6:H27');      

% for the GlobalTechCapital files             
caps1l_over_time = zeros(22,3);
caps1l_over_time(1:5,1) = total_capital_low(1,1)+storage(5);
caps1h_over_time = zeros(22,3);
caps1h_over_time(1:5,1) = total_capital_high(1,1)+storage(5);
caps1l_over_time(:,2) = 0.13;
caps1h_over_time(:,2) = 0.13;
caps1l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(1)));
caps1h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(1)));
caps1l_adv_over_time = caps1l_over_time;
caps1h_adv_over_time = caps1h_over_time;
for i = 6:22;
    caps1l_over_time(i,1) = caps1l_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    caps1h_over_time(i,1) = caps1h_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    caps1l_adv_over_time(i,1) = caps1l_adv_over_time(i-1,1)*0.75;
    caps1h_adv_over_time(i,1) = caps1h_adv_over_time(i-1,1)*0.75;
%     if i > 12; 
%         caps1l_over_time(i,1) = caps1l_over_time(i-1,1)*0.98; 
%         caps1h_over_time(i,1) = caps1h_over_time(i-1,1)*0.98; 
%     end
    if i > 8;
        caps1l_adv_over_time(i,1) = caps1l_adv_over_time(i-1,1)*0.95;
        caps1h_adv_over_time(i,1) = caps1h_adv_over_time(i-1,1)*0.95;
    end
end
caps1l_over_time(:,1) = caps1l_over_time(:,1)/to_gcam_dollars;
caps1h_over_time(:,1) = caps1h_over_time(:,1)/to_gcam_dollars;
caps1l_adv_over_time(:,1) = caps1l_adv_over_time(:,1)/to_gcam_dollars;
caps1h_adv_over_time(:,1) = caps1h_adv_over_time(:,1)/to_gcam_dollars;
xlswrite('L223.GlobalTechCapital_elec_owe1_low.csv',caps1l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe1_high.csv',caps1h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owe1_low_adv.csv',caps1l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe1_high_adv.csv',caps1h_adv_over_time,'F6:H27');      

caps2l_over_time = caps1l_over_time;
caps2h_over_time = caps1h_over_time;
caps2l_adv_over_time = caps1l_adv_over_time;
caps2h_adv_over_time = caps1h_adv_over_time;
caps2l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(2)));
caps2h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(2)));
caps2l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(2)));
caps2h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(2)));
xlswrite('L223.GlobalTechCapital_elec_owe2_low.csv',caps2l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe2_high.csv',caps2h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owe2_low_adv.csv',caps2l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe2_high_adv.csv',caps2h_adv_over_time,'F6:H27');      

caps3l_over_time = caps1l_over_time;
caps3h_over_time = caps1h_over_time;
caps3l_adv_over_time = caps1l_adv_over_time;
caps3h_adv_over_time = caps1h_adv_over_time;
caps3l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(3)));
caps3h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(3)));
caps3l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(3)));
caps3h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(3)));
xlswrite('L223.GlobalTechCapital_elec_owe3_low.csv',caps3l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe3_high.csv',caps3h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owe3_low_adv.csv',caps3l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe3_high_adv.csv',caps3h_adv_over_time,'F6:H27');      

caps4l_over_time = caps1l_over_time;
caps4h_over_time = caps1h_over_time;
caps4l_adv_over_time = caps1l_adv_over_time;
caps4h_adv_over_time = caps1h_adv_over_time;
caps4l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(4)));
caps4h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(4)));
caps4l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(4)));
caps4h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(4)));
xlswrite('L223.GlobalTechCapital_elec_owe4_low.csv',caps4l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe4_high.csv',caps4h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owe4_low_adv.csv',caps4l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe4_high_adv.csv',caps4h_adv_over_time,'F6:H27');      

caps5l_over_time = caps1l_over_time;
caps5h_over_time = caps1h_over_time;
caps5l_adv_over_time = caps1l_adv_over_time;
caps5h_adv_over_time = caps1h_adv_over_time;
caps5l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(5)));
caps5h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(5)));
caps5l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(5)));
caps5h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(5)));
xlswrite('L223.GlobalTechCapital_elec_owe5_low.csv',caps5l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe5_high.csv',caps5h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owe5_low_adv.csv',caps5l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe5_high_adv.csv',caps5h_adv_over_time,'F6:H27');      

caps6l_over_time = caps1l_over_time;
caps6h_over_time = caps1h_over_time;
caps6l_adv_over_time = caps1l_adv_over_time;
caps6h_adv_over_time = caps1h_adv_over_time;
caps6l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(6)));
caps6h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(6)));
caps6l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(6)));
caps6h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(6)));
xlswrite('L223.GlobalTechCapital_elec_owe6_low.csv',caps6l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe6_high.csv',caps6h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owe6_low_adv.csv',caps6l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe6_high_adv.csv',caps6h_adv_over_time,'F6:H27');      

caps7l_over_time = caps1l_over_time;
caps7h_over_time = caps1h_over_time;
caps7l_adv_over_time = caps1l_adv_over_time;
caps7h_adv_over_time = caps1h_adv_over_time;
caps7l_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(7)));
caps7h_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(7)));
caps7l_adv_over_time(:,3) = cf_high(find(wind_speeds == usa_bins_high(7)));
caps7h_adv_over_time(:,3) = cf_low(find(wind_speeds == usa_bins_low(7)));
xlswrite('L223.GlobalTechCapital_elec_owe7_low.csv',caps7l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe7_high.csv',caps7h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owe7_low_adv.csv',caps7l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owe7_high_adv.csv',caps7h_adv_over_time,'F6:H27');      

load onshore_omfixed
load onshore_omfixed_storage
omstorage = (onshore_omfixed_storage(:,1)-onshore_omfixed(:,1))*to_gcam_dollars;
omtech_change = zeros(22,1);
omtech_change_storage = zeros(22,1);
omstorage_change = zeros(22,1);
for i = 6:22;
    omstorage_change(i) = omstorage(i)/omstorage(5);
    omtech_change(i) = onshore_omfixed(i,1)/onshore_omfixed(5,1);
    omtech_change_storage(i) = onshore_omfixed_storage(i,1)/onshore_omfixed_storage(5,1);
end

% for the GlobalIntTechOMfixed files
om1l_over_time = zeros(22,2);
om1l_over_time(1:5,1) = om_low;
om1h_over_time = zeros(22,2);
om1h_over_time(1:5,1) = om_high;
om1l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(1)));
om1h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(1)));
om1l_adv_over_time = om1l_over_time;
om1h_adv_over_time = om1h_over_time;
for i = 6:22;
    om1l_over_time(i,1) = om1l_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    om1h_over_time(i,1) = om1h_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    om1l_adv_over_time(i,1) = om1l_adv_over_time(i-1,1)*0.75;
    om1h_adv_over_time(i,1) = om1h_adv_over_time(i-1,1)*0.75;
%     if i > 12; 
%         om1l_over_time(i,1) = om1l_over_time(i-1,1)*0.97; 
%         om1h_over_time(i,1) = om1h_over_time(i-1,1)*0.97;       
%     end
    if i > 8;
        om1l_adv_over_time(i,1) = om1l_adv_over_time(i-1,1)*0.95;
        om1h_adv_over_time(i,1) = om1h_adv_over_time(i-1,1)*0.95;
    end
end
om1l_over_time(:,1) = om1l_over_time(:,1)/to_gcam_dollars;
om1h_over_time(:,1) = om1h_over_time(:,1)/to_gcam_dollars;
om1l_adv_over_time(:,1) = om1l_adv_over_time(:,1)/to_gcam_dollars;
om1h_adv_over_time(:,1) = om1h_adv_over_time(:,1)/to_gcam_dollars;
xlswrite('L223.GlobalIntTechOMfixed_elec_owe1_low.csv',om1l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe1_high.csv',om1h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owe1_low_adv.csv',om1l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe1_high_adv.csv',om1h_adv_over_time,'F6:G27');      

om2l_over_time = om1l_over_time;
om2h_over_time = om1h_over_time;
om2l_adv_over_time = om1l_adv_over_time;
om2h_adv_over_time = om1h_adv_over_time;
om2l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(2)));
om2h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(2)));
om2l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(2)));
om2h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(2)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owe2_low.csv',om2l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe2_high.csv',om2h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owe2_low_adv.csv',om2l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe2_high_adv.csv',om2h_adv_over_time,'F6:G27');      

om3l_over_time = om1l_over_time;
om3h_over_time = om1h_over_time;
om3l_adv_over_time = om1l_adv_over_time;
om3h_adv_over_time = om1h_adv_over_time;
om3l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(3)));
om3h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(3)));
om3l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(3)));
om3h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(3)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owe3_low_adv.csv',om3l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe3_high_adv.csv',om3h_adv_over_time,'F6:G27');      

om4l_over_time = om1l_over_time;
om4h_over_time = om1h_over_time;
om4l_adv_over_time = om1l_adv_over_time;
om4h_adv_over_time = om1h_adv_over_time;
om4l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(4)));
om4h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(4)));
om4l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(4)));
om4h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(4)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owe4_low.csv',om4l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe4_high.csv',om4h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owe4_low_adv.csv',om4l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe4_high_adv.csv',om4h_adv_over_time,'F6:G27');      

om5l_over_time = om1l_over_time;
om5h_over_time = om1h_over_time;
om5l_adv_over_time = om1l_adv_over_time;
om5h_adv_over_time = om1h_adv_over_time;
om5l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(5)));
om5h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(5)));
om5l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(5)));
om5h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(5)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owe5_low.csv',om5l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe5_high.csv',om5h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owe5_low_adv.csv',om5l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe5_high_adv.csv',om5h_adv_over_time,'F6:G27');      

om6l_over_time = om1l_over_time;
om6h_over_time = om1h_over_time;
om6l_adv_over_time = om1l_adv_over_time;
om6h_adv_over_time = om1h_adv_over_time;
om6l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(6)));
om6h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(6)));
om6l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(6)));
om6h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(6)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owe6_low.csv',om6l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe6_high.csv',om6h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owe6_low_adv.csv',om6l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe6_high_adv.csv',om6h_adv_over_time,'F6:G27');      

om7l_over_time = om1l_over_time;
om7h_over_time = om1h_over_time;
om7l_adv_over_time = om1l_adv_over_time;
om7h_adv_over_time = om1h_adv_over_time;
om7l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(7)));
om7h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(7)));
om7l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(7)));
om7h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(7)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owe7_low.csv',om7l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe7_high.csv',om7h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owe7_low_adv.csv',om7l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owe7_high_adv.csv',om7h_adv_over_time,'F6:G27');      

% for the GlobalTechOMfixed files
oms1l_over_time = zeros(22,2);
oms1l_over_time(1:5,1) = om_low+omstorage(5);
oms1h_over_time = zeros(22,2);
oms1h_over_time(1:5,1) = om_high+omstorage(5);
oms1l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(1)));
oms1h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(1)));
oms1l_adv_over_time = oms1l_over_time;
oms1h_adv_over_time = oms1h_over_time;
for i = 6:22;
    oms1l_over_time(i,1) = oms1l_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    oms1h_over_time(i,1) = oms1h_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    oms1l_adv_over_time(i,1) = oms1l_adv_over_time(i-1,1)*0.75;
    oms1h_adv_over_time(i,1) = oms1h_adv_over_time(i-1,1)*0.75;
%     if i > 12; 
%         oms1l_over_time(i,1) = oms1l_over_time(i-1,1)*0.97; 
%         oms1h_over_time(i,1) = oms1h_over_time(i-1,1)*0.97; 
%     end
    if i > 8;
        oms1l_adv_over_time(i,1) = oms1l_adv_over_time(i-1,1)*0.95;
        oms1h_adv_over_time(i,1) = oms1h_adv_over_time(i-1,1)*0.95;
    end
end
xlswrite('L223.GlobalTechOMfixed_elec_owe1_low.csv',oms1l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe1_high.csv',oms1h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owe1_low_adv.csv',oms1l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe1_high_adv.csv',oms1h_adv_over_time,'F6:G27');      

oms2l_over_time = oms1l_over_time;
oms2h_over_time = oms1h_over_time;
oms2l_adv_over_time = oms1l_adv_over_time;
oms2h_adv_over_time = oms1h_adv_over_time;
oms2l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(2)));
oms2h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(2)));
oms2l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(2)));
oms2h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(2)));
xlswrite('L223.GlobalTechOMfixed_elec_owe2_low.csv',oms2l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe2_high.csv',oms2h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owe2_low_adv.csv',oms2l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe2_high_adv.csv',oms2h_adv_over_time,'F6:G27');      

oms3l_over_time = oms1l_over_time;
oms3h_over_time = oms1h_over_time;
oms3l_adv_over_time = oms1l_adv_over_time;
oms3h_adv_over_time = oms1h_adv_over_time;
oms3l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(3)));
oms3h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(3)));
oms3l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(3)));
oms3h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(3)));
xlswrite('L223.GlobalTechOMfixed_elec_owe3_low.csv',oms3l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe3_high.csv',oms3h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owe3_low_adv.csv',oms3l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe3_high_adv.csv',oms3h_adv_over_time,'F6:G27');      

oms4l_over_time = oms1l_over_time;
oms4h_over_time = oms1h_over_time;
oms4l_adv_over_time = oms1l_adv_over_time;
oms4h_adv_over_time = oms1h_adv_over_time;
oms4l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(4)));
oms4h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(4)));
oms4l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(4)));
oms4h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(4)));
xlswrite('L223.GlobalTechOMfixed_elec_owe4_low.csv',oms4l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe4_high.csv',oms4h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owe4_low_adv.csv',oms4l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe4_high_adv.csv',oms4h_adv_over_time,'F6:G27');      

oms5l_over_time = oms1l_over_time;
oms5h_over_time = oms1h_over_time;
oms5l_adv_over_time = oms1l_adv_over_time;
oms5h_adv_over_time = oms1h_adv_over_time;
oms5l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(5)));
oms5h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(5)));
oms5l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(5)));
oms5h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(5)));
xlswrite('L223.GlobalTechOMfixed_elec_owe5_low.csv',oms5l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe5_high.csv',oms5h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owe5_low_adv.csv',oms5l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe5_high_adv.csv',oms5h_adv_over_time,'F6:G27');      

oms6l_over_time = oms1l_over_time;
oms6h_over_time = oms1h_over_time;
oms6l_adv_over_time = oms1l_adv_over_time;
oms6h_adv_over_time = oms1h_adv_over_time;
oms6l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(6)));
oms6h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(6)));
oms6l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(6)));
oms6h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(6)));
xlswrite('L223.GlobalTechOMfixed_elec_owe6_low.csv',oms6l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe6_high.csv',oms6h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owe6_low_adv.csv',oms6l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe6_high_adv.csv',oms6h_adv_over_time,'F6:G27');      

oms7l_over_time = oms1l_over_time;
oms7h_over_time = oms1h_over_time;
oms7l_adv_over_time = oms1l_adv_over_time;
oms7h_adv_over_time = oms1h_adv_over_time;
oms7l_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(7)));
oms7h_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(7)));
oms7l_adv_over_time(:,2) = cf_high(find(wind_speeds == usa_bins_high(7)));
oms7h_adv_over_time(:,2) = cf_low(find(wind_speeds == usa_bins_low(7)));
xlswrite('L223.GlobalTechOMfixed_elec_owe7_low.csv',oms7l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe7_high.csv',oms7h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owe7_low_adv.csv',oms7l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owe7_high_adv.csv',oms7h_adv_over_time,'F6:G27');      

cf_1_low = zeros(26*22*2,1); cf_1_high = cf_1_low;
cf_1_high(:,1) = cf_low(find(wind_speeds == usa_bins_low(1)));
cf_1_low(:,1) = cf_high(find(wind_speeds == usa_bins_high(1)));
xlswrite('L223.StubTechCapFactor_elec_owe1_usa_low.csv',cf_1_low,'G6:G1149')
xlswrite('L223.StubTechCapFactor_elec_owe1_usa_high.csv',cf_1_high,'G6:G1149')
xlswrite('L223.StubTechCapFactor_elec_owe1_usa_low.csv',cf_1_low,'I6:I1149')
xlswrite('L223.StubTechCapFactor_elec_owe1_usa_high.csv',cf_1_high,'I6:I1149')

cf_2_low = zeros(26*22*2,1); cf_2_high = cf_2_low;
cf_2_high(:,1) = cf_low(find(wind_speeds == usa_bins_low(2)));
cf_2_low(:,1) = cf_high(find(wind_speeds == usa_bins_high(2)));
xlswrite('L223.StubTechCapFactor_elec_owe2_usa_low.csv',cf_2_low,'G6:G1149')
xlswrite('L223.StubTechCapFactor_elec_owe2_usa_high.csv',cf_2_high,'G6:G1149')
xlswrite('L223.StubTechCapFactor_elec_owe2_usa_low.csv',cf_2_low,'I6:I1149')
xlswrite('L223.StubTechCapFactor_elec_owe2_usa_high.csv',cf_2_high,'I6:I1149')

cf_3_low = zeros(24*22*2,1); cf_3_high = cf_3_low;
cf_3_high(:,1) = cf_low(find(wind_speeds == usa_bins_low(3)));
cf_3_low(:,1) = cf_high(find(wind_speeds == usa_bins_high(3)));
xlswrite('L223.StubTechCapFactor_elec_owe3_usa_low.csv',cf_3_low,'G6:G1061')
xlswrite('L223.StubTechCapFactor_elec_owe3_usa_high.csv',cf_3_high,'G6:G1061')
xlswrite('L223.StubTechCapFactor_elec_owe3_usa_low.csv',cf_3_low,'I6:I1061')
xlswrite('L223.StubTechCapFactor_elec_owe3_usa_high.csv',cf_3_high,'I6:I1061')

cf_4_low = zeros(18*22*2,1); cf_4_high = cf_4_low;
cf_4_high(:,1) = cf_low(find(wind_speeds == usa_bins_low(4)));
cf_4_low(:,1) = cf_high(find(wind_speeds == usa_bins_high(4)));
xlswrite('L223.StubTechCapFactor_elec_owe4_usa_low.csv',cf_4_low,'G6:G797')
xlswrite('L223.StubTechCapFactor_elec_owe4_usa_high.csv',cf_4_high,'G6:G797')
xlswrite('L223.StubTechCapFactor_elec_owe4_usa_low.csv',cf_4_low,'I6:I797')
xlswrite('L223.StubTechCapFactor_elec_owe4_usa_high.csv',cf_4_high,'I6:I797')

cf_5_low = zeros(10*22*2,1); cf_5_high = cf_5_low;
cf_5_high(:,1) = cf_low(find(wind_speeds == usa_bins_low(5)));
cf_5_low(:,1) = cf_high(find(wind_speeds == usa_bins_high(5)));
xlswrite('L223.StubTechCapFactor_elec_owe5_usa_low.csv',cf_5_low,'G6:G445')
xlswrite('L223.StubTechCapFactor_elec_owe5_usa_high.csv',cf_5_high,'G6:G445')
xlswrite('L223.StubTechCapFactor_elec_owe5_usa_low.csv',cf_5_low,'I6:I445')
xlswrite('L223.StubTechCapFactor_elec_owe5_usa_high.csv',cf_5_high,'I6:I445')

cf_6_low = zeros(7*22*2,1); cf_6_high = cf_6_low;
cf_6_high(:,1) = cf_low(find(wind_speeds == usa_bins_low(6)));
cf_6_low(:,1) = cf_high(find(wind_speeds == usa_bins_high(6)));
xlswrite('L223.StubTechCapFactor_elec_owe6_usa_low.csv',cf_6_low,'G6:G313')
xlswrite('L223.StubTechCapFactor_elec_owe6_usa_high.csv',cf_6_high,'G6:G313')
xlswrite('L223.StubTechCapFactor_elec_owe6_usa_low.csv',cf_6_low,'I6:I313')
xlswrite('L223.StubTechCapFactor_elec_owe6_usa_high.csv',cf_6_high,'I6:I313')

cf_7_low = zeros(4*22*2,1); cf_7_high = cf_7_low;
cf_7_high(:,1) = cf_low(find(wind_speeds == usa_bins_low(7)));
cf_7_low(:,1) = cf_high(find(wind_speeds == usa_bins_high(7)));
xlswrite('L223.StubTechCapFactor_elec_owe7_usa_low.csv',cf_7_low,'G6:G181')
xlswrite('L223.StubTechCapFactor_elec_owe7_usa_high.csv',cf_7_high,'G6:G181')
xlswrite('L223.StubTechCapFactor_elec_owe7_usa_low.csv',cf_7_low,'I6:G181')
xlswrite('L223.StubTechCapFactor_elec_owe7_usa_high.csv',cf_7_high,'I6:G181')

owec4_global_high = [Africa_Eastern_c4_low_h_cum(end),Africa_Eastern_mid(3),Africa_Eastern_exp(3);
                 Africa_Northern_c4_low_h_cum(end),Africa_Northern_mid(3),Africa_Northern_exp(3);
                 Africa_Southern_c4_low_h_cum(end),Africa_Southern_mid(3),Africa_Southern_exp(3);
                 Africa_Western_c4_low_h_cum(end),Africa_Western_mid(3),Africa_Western_exp(3);
                 Australia_NZ_c4_low_h_cum(end),Australia_NZ_mid(3),Australia_NZ_exp(3);
                 Brazil_c4_low_h_cum(end),Brazil_mid(3),Brazil_exp(3);
                 Canada_c4_low_h_cum(end),Canada_mid(3),Canada_exp(3);
                 Central_America_and_Caribbean_c4_low_h_cum(end),Central_America_and_Caribbean_mid(3),Central_America_and_Caribbean_exp(3);
                 Central_Asia_c4_low_h_cum(end),Central_Asia_mid(3),Central_Asia_exp(3);
                 China_c4_low_h_cum(end),China_mid(3),China_exp(3);
                 EU_12_c4_low_h_cum(end),EU_12_mid(3),EU_12_exp(3);
                 EU_15_c4_low_h_cum(end),EU_15_mid(3),EU_15_exp(3);
                 Europe_Eastern_c4_low_h_cum(end),Europe_Eastern_mid(3),Europe_Eastern_exp(3);
                 Europe_Non_EU_c4_low_h_cum(end),Europe_Non_EU_mid(3),Europe_Non_EU_exp(3);
                 European_Free_Trade_Association_c4_low_h_cum(end),European_Free_Trade_Association_mid(3),European_Free_Trade_Association_exp(3);
                 India_c4_low_h_cum(end),India_mid(3),India_exp(3);
                 Indonesia_c4_low_h_cum(end),Indonesia_mid(3),Indonesia_exp(3);
                 Japan_c4_low_h_cum(end),Japan_mid(3),Japan_exp(3);
                 Mexico_c4_low_h_cum(end),Mexico_mid(3),Mexico_exp(3);
                 Middle_East_c4_low_h_cum(end),Middle_East_mid(3),Middle_East_exp(3);
                 Pakistan_c4_low_h_cum(end),Pakistan_mid(3),Pakistan_exp(3);
                 Russia_c4_low_h_cum(end),Russia_mid(3),Russia_exp(3);
                 South_Africa_c4_low_h_cum(end),South_Africa_mid(3),South_Africa_exp(3);
                 South_America_Northern_c4_low_h_cum(end),South_America_Northern_mid(3),South_America_Northern_exp(3);
                 South_America_Southern_c4_low_h_cum(end),South_America_Southern_mid(3),South_America_Southern_exp(3);
                 South_Asia_c4_low_h_cum(end),South_Asia_mid(3),South_Asia_exp(3)
                 South_Korea_c4_low_h_cum(end),South_Korea_mid(3),South_Korea_exp(3)
                 Southeast_Asia_c4_low_h_cum(end),Southeast_Asia_mid(3),Southeast_Asia_exp(3)
                 Taiwan_c4_low_h_cum(end),Taiwan_mid(3),Taiwan_exp(3)
                 Argentina_c4_low_h_cum(end),Argentina_mid(3),Argentina_exp(3)
                 Columbia_c4_low_h_cum(end),Columbia_mid(3),Columbia_exp(3)];
owec4_global_high_mod = owec4_global_high(owec4_global_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec4_smooth_high_mod.csv', owec4_global_high_mod,'D6:F35')

owec5_global_high = [Africa_Eastern_c5_low_h_cum(end),Africa_Eastern_mid(7),Africa_Eastern_exp(7);
                 Africa_Northern_c5_low_h_cum(end),Africa_Northern_mid(7),Africa_Northern_exp(7);
                 Africa_Southern_c5_low_h_cum(end),Africa_Southern_mid(7),Africa_Southern_exp(7);
                 Africa_Western_c5_low_h_cum(end),Africa_Western_mid(7),Africa_Western_exp(7);
                 Australia_NZ_c5_low_h_cum(end),Australia_NZ_mid(7),Australia_NZ_exp(7);
                 Brazil_c5_low_h_cum(end),Brazil_mid(7),Brazil_exp(7);
                 Canada_c5_low_h_cum(end),Canada_mid(7),Canada_exp(7);
                 Central_America_and_Caribbean_c5_low_h_cum(end),Central_America_and_Caribbean_mid(7),Central_America_and_Caribbean_exp(7);
                 Central_Asia_c5_low_h_cum(end),Central_Asia_mid(7),Central_Asia_exp(7);
                 China_c5_low_h_cum(end),China_mid(7),China_exp(7);
                 EU_12_c5_low_h_cum(end),EU_12_mid(7),EU_12_exp(7);
                 EU_15_c5_low_h_cum(end),EU_15_mid(7),EU_15_exp(7);
                 Europe_Eastern_c5_low_h_cum(end),Europe_Eastern_mid(7),Europe_Eastern_exp(7);
                 Europe_Non_EU_c5_low_h_cum(end),Europe_Non_EU_mid(7),Europe_Non_EU_exp(7);
                 European_Free_Trade_Association_c5_low_h_cum(end),European_Free_Trade_Association_mid(7),European_Free_Trade_Association_exp(7);
                 India_c5_low_h_cum(end),India_mid(7),India_exp(7);
                 Indonesia_c5_low_h_cum(end),Indonesia_mid(7),Indonesia_exp(7);
                 Japan_c5_low_h_cum(end),Japan_mid(7),Japan_exp(7);
                 Mexico_c5_low_h_cum(end),Mexico_mid(7),Mexico_exp(7);
                 Middle_East_c5_low_h_cum(end),Middle_East_mid(7),Middle_East_exp(7);
                 Pakistan_c5_low_h_cum(end),Pakistan_mid(7),Pakistan_exp(7);
                 Russia_c5_low_h_cum(end),Russia_mid(7),Russia_exp(7);
                 South_Africa_c5_low_h_cum(end),South_Africa_mid(7),South_Africa_exp(7);
                 South_America_Northern_c5_low_h_cum(end),South_America_Northern_mid(7),South_America_Northern_exp(7);
                 South_America_Southern_c5_low_h_cum(end),South_America_Southern_mid(7),South_America_Southern_exp(7);
                 South_Asia_c5_low_h_cum(end),South_Asia_mid(7),South_Asia_exp(7)
                 South_Korea_c5_low_h_cum(end),South_Korea_mid(7),South_Korea_exp(7)
                 Southeast_Asia_c5_low_h_cum(end),Southeast_Asia_mid(7),Southeast_Asia_exp(7)
                 Taiwan_c5_low_h_cum(end),Taiwan_mid(7),Taiwan_exp(7)
                 Argentina_c5_low_h_cum(end),Argentina_mid(7),Argentina_exp(7)
                 Columbia_c5_low_h_cum(end),Columbia_mid(7),Columbia_exp(7)];
owec5_global_high_mod = owec5_global_high(owec5_global_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec5_smooth_high_mod.csv', owec5_global_high_mod,'D6:F35')

owec6_global_high = [Africa_Eastern_c6_low_h_cum(end),Africa_Eastern_mid(11),Africa_Eastern_exp(11);
                 Africa_Northern_c6_low_h_cum(end),Africa_Northern_mid(11),Africa_Northern_exp(11);
                 Africa_Southern_c6_low_h_cum(end),Africa_Southern_mid(11),Africa_Southern_exp(11);
                 Africa_Western_c6_low_h_cum(end),Africa_Western_mid(11),Africa_Western_exp(11);
                 Australia_NZ_c6_low_h_cum(end),Australia_NZ_mid(11),Australia_NZ_exp(11);
                 Brazil_c6_low_h_cum(end),Brazil_mid(11),Brazil_exp(11);
                 Canada_c6_low_h_cum(end),Canada_mid(11),Canada_exp(11);
                 Central_America_and_Caribbean_c6_low_h_cum(end),Central_America_and_Caribbean_mid(11),Central_America_and_Caribbean_exp(11);
                 Central_Asia_c6_low_h_cum(end),Central_Asia_mid(11),Central_Asia_exp(11);
                 China_c6_low_h_cum(end),China_mid(11),China_exp(11);
                 EU_12_c6_low_h_cum(end),EU_12_mid(11),EU_12_exp(11);
                 EU_15_c6_low_h_cum(end),EU_15_mid(11),EU_15_exp(11);
                 Europe_Eastern_c6_low_h_cum(end),Europe_Eastern_mid(11),Europe_Eastern_exp(11);
                 Europe_Non_EU_c6_low_h_cum(end),Europe_Non_EU_mid(11),Europe_Non_EU_exp(11);
                 European_Free_Trade_Association_c6_low_h_cum(end),European_Free_Trade_Association_mid(11),European_Free_Trade_Association_exp(11);
                 India_c6_low_h_cum(end),India_mid(11),India_exp(11);
                 Indonesia_c6_low_h_cum(end),Indonesia_mid(11),Indonesia_exp(11);
                 Japan_c6_low_h_cum(end),Japan_mid(11),Japan_exp(11);
                 Mexico_c6_low_h_cum(end),Mexico_mid(11),Mexico_exp(11);
                 Middle_East_c6_low_h_cum(end),Middle_East_mid(11),Middle_East_exp(11);
                 Pakistan_c6_low_h_cum(end),Pakistan_mid(11),Pakistan_exp(11);
                 Russia_c6_low_h_cum(end),Russia_mid(11),Russia_exp(11);
                 South_Africa_c6_low_h_cum(end),South_Africa_mid(11),South_Africa_exp(11);
                 South_America_Northern_c6_low_h_cum(end),South_America_Northern_mid(11),South_America_Northern_exp(11);
                 South_America_Southern_c6_low_h_cum(end),South_America_Southern_mid(11),South_America_Southern_exp(11);
                 South_Asia_c6_low_h_cum(end),South_Asia_mid(11),South_Asia_exp(11)
                 South_Korea_c6_low_h_cum(end),South_Korea_mid(11),South_Korea_exp(11)
                 Southeast_Asia_c6_low_h_cum(end),Southeast_Asia_mid(11),Southeast_Asia_exp(11)
                 Taiwan_c6_low_h_cum(end),Taiwan_mid(11),Taiwan_exp(11)
                 Argentina_c6_low_h_cum(end),Argentina_mid(11),Argentina_exp(11)
                 Columbia_c6_low_h_cum(end),Columbia_mid(11),Columbia_exp(11)];
owec6_global_high_mod = owec6_global_high(owec6_global_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec6_smooth_high_mod.csv', owec6_global_high_mod,'D6:F35')

owec7_global_high = [Africa_Eastern_c7_low_h_cum(end),Africa_Eastern_mid(15),Africa_Eastern_exp(15);
                 Africa_Northern_c7_low_h_cum(end),Africa_Northern_mid(15),Africa_Northern_exp(15);
                 Africa_Southern_c7_low_h_cum(end),Africa_Southern_mid(15),Africa_Southern_exp(15);
                 Africa_Western_c7_low_h_cum(end),Africa_Western_mid(15),Africa_Western_exp(15);
                 Australia_NZ_c7_low_h_cum(end),Australia_NZ_mid(15),Australia_NZ_exp(15);
                 Brazil_c7_low_h_cum(end),Brazil_mid(15),Brazil_exp(15);
                 Canada_c7_low_h_cum(end),Canada_mid(15),Canada_exp(15);
                 Central_America_and_Caribbean_c7_low_h_cum(end),Central_America_and_Caribbean_mid(15),Central_America_and_Caribbean_exp(15);
                 Central_Asia_c7_low_h_cum(end),Central_Asia_mid(15),Central_Asia_exp(15);
                 China_c7_low_h_cum(end),China_mid(15),China_exp(15);
                 EU_12_c7_low_h_cum(end),EU_12_mid(15),EU_12_exp(15);
                 EU_15_c7_low_h_cum(end),EU_15_mid(15),EU_15_exp(15);
                 Europe_Eastern_c7_low_h_cum(end),Europe_Eastern_mid(15),Europe_Eastern_exp(15);
                 Europe_Non_EU_c7_low_h_cum(end),Europe_Non_EU_mid(15),Europe_Non_EU_exp(15);
                 European_Free_Trade_Association_c7_low_h_cum(end),European_Free_Trade_Association_mid(15),European_Free_Trade_Association_exp(15);
                 India_c7_low_h_cum(end),India_mid(15),India_exp(15);
                 Indonesia_c7_low_h_cum(end),Indonesia_mid(15),Indonesia_exp(15);
                 Japan_c7_low_h_cum(end),Japan_mid(15),Japan_exp(15);
                 Mexico_c7_low_h_cum(end),Mexico_mid(15),Mexico_exp(15);
                 Middle_East_c7_low_h_cum(end),Middle_East_mid(15),Middle_East_exp(15);
                 Pakistan_c7_low_h_cum(end),Pakistan_mid(15),Pakistan_exp(15);
                 Russia_c7_low_h_cum(end),Russia_mid(15),Russia_exp(15);
                 South_Africa_c7_low_h_cum(end),South_Africa_mid(15),South_Africa_exp(15);
                 South_America_Northern_c7_low_h_cum(end),South_America_Northern_mid(15),South_America_Northern_exp(15);
                 South_America_Southern_c7_low_h_cum(end),South_America_Southern_mid(15),South_America_Southern_exp(15);
                 South_Asia_c7_low_h_cum(end),South_Asia_mid(15),South_Asia_exp(15)
                 South_Korea_c7_low_h_cum(end),South_Korea_mid(15),South_Korea_exp(15)
                 Southeast_Asia_c7_low_h_cum(end),Southeast_Asia_mid(15),Southeast_Asia_exp(15)
                 Taiwan_c7_low_h_cum(end),Taiwan_mid(15),Taiwan_exp(15)
                 Argentina_c7_low_h_cum(end),Argentina_mid(15),Argentina_exp(15)
                 Columbia_c7_low_h_cum(end),Columbia_mid(15),Columbia_exp(15)];
owec7_global_high_mod = owec7_global_high(owec7_global_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec7_smooth_high_mod.csv', owec7_global_high_mod,'D6:F35')

owec89_global_high = [Africa_Eastern_c89_low_h_cum(end),Africa_Eastern_mid(19),Africa_Eastern_exp(19);
                 Africa_Northern_c89_low_h_cum(end),Africa_Northern_mid(19),Africa_Northern_exp(19);
                 Africa_Southern_c89_low_h_cum(end),Africa_Southern_mid(19),Africa_Southern_exp(19);
                 Africa_Western_c89_low_h_cum(end),Africa_Western_mid(19),Africa_Western_exp(19);
                 Australia_NZ_c89_low_h_cum(end),Australia_NZ_mid(19),Australia_NZ_exp(19);
                 Brazil_c89_low_h_cum(end),Brazil_mid(19),Brazil_exp(19);
                 Canada_c89_low_h_cum(end),Canada_mid(19),Canada_exp(19);
                 Central_America_and_Caribbean_c89_low_h_cum(end),Central_America_and_Caribbean_mid(19),Central_America_and_Caribbean_exp(19);
                 Central_Asia_c89_low_h_cum(end),Central_Asia_mid(19),Central_Asia_exp(19);
                 China_c89_low_h_cum(end),China_mid(19),China_exp(19);
                 EU_12_c89_low_h_cum(end),EU_12_mid(19),EU_12_exp(19);
                 EU_15_c89_low_h_cum(end),EU_15_mid(19),EU_15_exp(19);
                 Europe_Eastern_c89_low_h_cum(end),Europe_Eastern_mid(19),Europe_Eastern_exp(19);
                 Europe_Non_EU_c89_low_h_cum(end),Europe_Non_EU_mid(19),Europe_Non_EU_exp(19);
                 European_Free_Trade_Association_c89_low_h_cum(end),European_Free_Trade_Association_mid(19),European_Free_Trade_Association_exp(19);
                 India_c89_low_h_cum(end),India_mid(19),India_exp(19);
                 Indonesia_c89_low_h_cum(end),Indonesia_mid(19),Indonesia_exp(19);
                 Japan_c89_low_h_cum(end),Japan_mid(19),Japan_exp(19);
                 Mexico_c89_low_h_cum(end),Mexico_mid(19),Mexico_exp(19);
                 Middle_East_c89_low_h_cum(end),Middle_East_mid(19),Middle_East_exp(19);
                 Pakistan_c89_low_h_cum(end),Pakistan_mid(19),Pakistan_exp(19);
                 Russia_c89_low_h_cum(end),Russia_mid(19),Russia_exp(19);
                 South_Africa_c89_low_h_cum(end),South_Africa_mid(19),South_Africa_exp(19);
                 South_America_Northern_c89_low_h_cum(end),South_America_Northern_mid(19),South_America_Northern_exp(19);
                 South_America_Southern_c89_low_h_cum(end),South_America_Southern_mid(19),South_America_Southern_exp(19);
                 South_Asia_c89_low_h_cum(end),South_Asia_mid(19),South_Asia_exp(19)
                 South_Korea_c89_low_h_cum(end),South_Korea_mid(19),South_Korea_exp(19)
                 Southeast_Asia_c89_low_h_cum(end),Southeast_Asia_mid(19),Southeast_Asia_exp(19)
                 Taiwan_c89_low_h_cum(end),Taiwan_mid(19),Taiwan_exp(19)
                 Argentina_c89_low_h_cum(end),Argentina_mid(19),Argentina_exp(19)
                 Columbia_c89_low_h_cum(end),Columbia_mid(19),Columbia_exp(19)];
owec89_global_high_mod = owec89_global_high(owec89_global_high(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec89_smooth_high_mod.csv', owec89_global_high_mod,'D6:F35')

owec4_global_low = [Africa_Eastern_c4_high_l_cum(end),Africa_Eastern_mid(2),Africa_Eastern_exp(2);
                 Africa_Northern_c4_high_l_cum(end),Africa_Northern_mid(2),Africa_Northern_exp(2);
                 Africa_Southern_c4_high_l_cum(end),Africa_Southern_mid(2),Africa_Southern_exp(2);
                 Africa_Western_c4_high_l_cum(end),Africa_Western_mid(2),Africa_Western_exp(2);
                 Australia_NZ_c4_high_l_cum(end),Australia_NZ_mid(2),Australia_NZ_exp(2);
                 Brazil_c4_high_l_cum(end),Brazil_mid(2),Brazil_exp(2);
                 Canada_c4_high_l_cum(end),Canada_mid(2),Canada_exp(2);
                 Central_America_and_Caribbean_c4_high_l_cum(end),Central_America_and_Caribbean_mid(2),Central_America_and_Caribbean_exp(2);
                 Central_Asia_c4_high_l_cum(end),Central_Asia_mid(2),Central_Asia_exp(2);
                 China_c4_high_l_cum(end),China_mid(2),China_exp(2);
                 EU_12_c4_high_l_cum(end),EU_12_mid(2),EU_12_exp(2);
                 EU_15_c4_high_l_cum(end),EU_15_mid(2),EU_15_exp(2);
                 Europe_Eastern_c4_high_l_cum(end),Europe_Eastern_mid(2),Europe_Eastern_exp(2);
                 Europe_Non_EU_c4_high_l_cum(end),Europe_Non_EU_mid(2),Europe_Non_EU_exp(2);
                 European_Free_Trade_Association_c4_high_l_cum(end),European_Free_Trade_Association_mid(2),European_Free_Trade_Association_exp(2);
                 India_c4_high_l_cum(end),India_mid(2),India_exp(2);
                 Indonesia_c4_high_l_cum(end),Indonesia_mid(2),Indonesia_exp(2);
                 Japan_c4_high_l_cum(end),Japan_mid(2),Japan_exp(2);
                 Mexico_c4_high_l_cum(end),Mexico_mid(2),Mexico_exp(2);
                 Middle_East_c4_high_l_cum(end),Middle_East_mid(2),Middle_East_exp(2);
                 Pakistan_c4_high_l_cum(end),Pakistan_mid(2),Pakistan_exp(2);
                 Russia_c4_high_l_cum(end),Russia_mid(2),Russia_exp(2);
                 South_Africa_c4_high_l_cum(end),South_Africa_mid(2),South_Africa_exp(2);
                 South_America_Northern_c4_high_l_cum(end),South_America_Northern_mid(2),South_America_Northern_exp(2);
                 South_America_Southern_c4_high_l_cum(end),South_America_Southern_mid(2),South_America_Southern_exp(2);
                 South_Asia_c4_high_l_cum(end),South_Asia_mid(2),South_Asia_exp(2)
                 South_Korea_c4_high_l_cum(end),South_Korea_mid(2),South_Korea_exp(2)
                 Southeast_Asia_c4_high_l_cum(end),Southeast_Asia_mid(2),Southeast_Asia_exp(2)
                 Taiwan_c4_high_l_cum(end),Taiwan_mid(2),Taiwan_exp(2)
                 Argentina_c4_high_l_cum(end),Argentina_mid(2),Argentina_exp(2)
                 Columbia_c4_high_l_cum(end),Columbia_mid(2),Columbia_exp(2)];
owec4_global_low_mod = owec4_global_low(owec4_global_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec4_smooth_low_mod.csv', owec4_global_low_mod,'D6:F35')

owec5_global_low = [Africa_Eastern_c5_high_l_cum(end),Africa_Eastern_mid(6),Africa_Eastern_exp(6);
                 Africa_Northern_c5_high_l_cum(end),Africa_Northern_mid(6),Africa_Northern_exp(6);
                 Africa_Southern_c5_high_l_cum(end),Africa_Southern_mid(6),Africa_Southern_exp(6);
                 Africa_Western_c5_high_l_cum(end),Africa_Western_mid(6),Africa_Western_exp(6);
                 Australia_NZ_c5_high_l_cum(end),Australia_NZ_mid(6),Australia_NZ_exp(6);
                 Brazil_c5_high_l_cum(end),Brazil_mid(6),Brazil_exp(6);
                 Canada_c5_high_l_cum(end),Canada_mid(6),Canada_exp(6);
                 Central_America_and_Caribbean_c5_high_l_cum(end),Central_America_and_Caribbean_mid(6),Central_America_and_Caribbean_exp(6);
                 Central_Asia_c5_high_l_cum(end),Central_Asia_mid(6),Central_Asia_exp(6);
                 China_c5_high_l_cum(end),China_mid(6),China_exp(6);
                 EU_12_c5_high_l_cum(end),EU_12_mid(6),EU_12_exp(6);
                 EU_15_c5_high_l_cum(end),EU_15_mid(6),EU_15_exp(6);
                 Europe_Eastern_c5_high_l_cum(end),Europe_Eastern_mid(6),Europe_Eastern_exp(6);
                 Europe_Non_EU_c5_high_l_cum(end),Europe_Non_EU_mid(6),Europe_Non_EU_exp(6);
                 European_Free_Trade_Association_c5_high_l_cum(end),European_Free_Trade_Association_mid(6),European_Free_Trade_Association_exp(6);
                 India_c5_high_l_cum(end),India_mid(6),India_exp(6);
                 Indonesia_c5_high_l_cum(end),Indonesia_mid(6),Indonesia_exp(6);
                 Japan_c5_high_l_cum(end),Japan_mid(6),Japan_exp(6);
                 Mexico_c5_high_l_cum(end),Mexico_mid(6),Mexico_exp(6);
                 Middle_East_c5_high_l_cum(end),Middle_East_mid(6),Middle_East_exp(6);
                 Pakistan_c5_high_l_cum(end),Pakistan_mid(6),Pakistan_exp(6);
                 Russia_c5_high_l_cum(end),Russia_mid(6),Russia_exp(6);
                 South_Africa_c5_high_l_cum(end),South_Africa_mid(6),South_Africa_exp(6);
                 South_America_Northern_c5_high_l_cum(end),South_America_Northern_mid(6),South_America_Northern_exp(6);
                 South_America_Southern_c5_high_l_cum(end),South_America_Southern_mid(6),South_America_Southern_exp(6);
                 South_Asia_c5_high_l_cum(end),South_Asia_mid(6),South_Asia_exp(6)
                 South_Korea_c5_high_l_cum(end),South_Korea_mid(6),South_Korea_exp(6)
                 Southeast_Asia_c5_high_l_cum(end),Southeast_Asia_mid(6),Southeast_Asia_exp(6)
                 Taiwan_c5_high_l_cum(end),Taiwan_mid(6),Taiwan_exp(6)
                 Argentina_c5_high_l_cum(end),Argentina_mid(6),Argentina_exp(6)
                 Columbia_c5_high_l_cum(end),Columbia_mid(6),Columbia_exp(6)];
owec5_global_low_mod = owec5_global_low(owec5_global_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec5_smooth_low_mod.csv', owec5_global_low_mod,'D6:F35')

owec6_global_low = [Africa_Eastern_c6_high_l_cum(end),Africa_Eastern_mid(10),Africa_Eastern_exp(10);
                 Africa_Northern_c6_high_l_cum(end),Africa_Northern_mid(10),Africa_Northern_exp(10);
                 Africa_Southern_c6_high_l_cum(end),Africa_Southern_mid(10),Africa_Southern_exp(10);
                 Africa_Western_c6_high_l_cum(end),Africa_Western_mid(10),Africa_Western_exp(10);
                 Australia_NZ_c6_high_l_cum(end),Australia_NZ_mid(10),Australia_NZ_exp(10);
                 Brazil_c6_high_l_cum(end),Brazil_mid(10),Brazil_exp(10);
                 Canada_c6_high_l_cum(end),Canada_mid(10),Canada_exp(10);
                 Central_America_and_Caribbean_c6_high_l_cum(end),Central_America_and_Caribbean_mid(10),Central_America_and_Caribbean_exp(10);
                 Central_Asia_c6_high_l_cum(end),Central_Asia_mid(10),Central_Asia_exp(10);
                 China_c6_high_l_cum(end),China_mid(10),China_exp(10);
                 EU_12_c6_high_l_cum(end),EU_12_mid(10),EU_12_exp(10);
                 EU_15_c6_high_l_cum(end),EU_15_mid(10),EU_15_exp(10);
                 Europe_Eastern_c6_high_l_cum(end),Europe_Eastern_mid(10),Europe_Eastern_exp(10);
                 Europe_Non_EU_c6_high_l_cum(end),Europe_Non_EU_mid(10),Europe_Non_EU_exp(10);
                 European_Free_Trade_Association_c6_high_l_cum(end),European_Free_Trade_Association_mid(10),European_Free_Trade_Association_exp(10);
                 India_c6_high_l_cum(end),India_mid(10),India_exp(10);
                 Indonesia_c6_high_l_cum(end),Indonesia_mid(10),Indonesia_exp(10);
                 Japan_c6_high_l_cum(end),Japan_mid(10),Japan_exp(10);
                 Mexico_c6_high_l_cum(end),Mexico_mid(10),Mexico_exp(10);
                 Middle_East_c6_high_l_cum(end),Middle_East_mid(10),Middle_East_exp(10);
                 Pakistan_c6_high_l_cum(end),Pakistan_mid(10),Pakistan_exp(10);
                 Russia_c6_high_l_cum(end),Russia_mid(10),Russia_exp(10);
                 South_Africa_c6_high_l_cum(end),South_Africa_mid(10),South_Africa_exp(10);
                 South_America_Northern_c6_high_l_cum(end),South_America_Northern_mid(10),South_America_Northern_exp(10);
                 South_America_Southern_c6_high_l_cum(end),South_America_Southern_mid(10),South_America_Southern_exp(10);
                 South_Asia_c6_high_l_cum(end),South_Asia_mid(10),South_Asia_exp(10)
                 South_Korea_c6_high_l_cum(end),South_Korea_mid(10),South_Korea_exp(10)
                 Southeast_Asia_c6_high_l_cum(end),Southeast_Asia_mid(10),Southeast_Asia_exp(10)
                 Taiwan_c6_high_l_cum(end),Taiwan_mid(10),Taiwan_exp(10)
                 Argentina_c6_high_l_cum(end),Argentina_mid(10),Argentina_exp(10)
                 Columbia_c6_high_l_cum(end),Columbia_mid(10),Columbia_exp(10)];
owec6_global_low_mod = owec6_global_low(owec6_global_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec6_smooth_low_mod.csv', owec6_global_low_mod,'D6:F35')

owec7_global_low = [Africa_Eastern_c7_high_l_cum(end),Africa_Eastern_mid(14),Africa_Eastern_exp(14);
                 Africa_Northern_c7_high_l_cum(end),Africa_Northern_mid(14),Africa_Northern_exp(14);
                 Africa_Southern_c7_high_l_cum(end),Africa_Southern_mid(14),Africa_Southern_exp(14);
                 Africa_Western_c7_high_l_cum(end),Africa_Western_mid(14),Africa_Western_exp(14);
                 Australia_NZ_c7_high_l_cum(end),Australia_NZ_mid(14),Australia_NZ_exp(14);
                 Brazil_c7_high_l_cum(end),Brazil_mid(14),Brazil_exp(14);
                 Canada_c7_high_l_cum(end),Canada_mid(14),Canada_exp(14);
                 Central_America_and_Caribbean_c7_high_l_cum(end),Central_America_and_Caribbean_mid(14),Central_America_and_Caribbean_exp(14);
                 Central_Asia_c7_high_l_cum(end),Central_Asia_mid(14),Central_Asia_exp(14);
                 China_c7_high_l_cum(end),China_mid(14),China_exp(14);
                 EU_12_c7_high_l_cum(end),EU_12_mid(14),EU_12_exp(14);
                 EU_15_c7_high_l_cum(end),EU_15_mid(14),EU_15_exp(14);
                 Europe_Eastern_c7_high_l_cum(end),Europe_Eastern_mid(14),Europe_Eastern_exp(14);
                 Europe_Non_EU_c7_high_l_cum(end),Europe_Non_EU_mid(14),Europe_Non_EU_exp(14);
                 European_Free_Trade_Association_c7_high_l_cum(end),European_Free_Trade_Association_mid(14),European_Free_Trade_Association_exp(14);
                 India_c7_high_l_cum(end),India_mid(14),India_exp(14);
                 Indonesia_c7_high_l_cum(end),Indonesia_mid(14),Indonesia_exp(14);
                 Japan_c7_high_l_cum(end),Japan_mid(14),Japan_exp(14);
                 Mexico_c7_high_l_cum(end),Mexico_mid(14),Mexico_exp(14);
                 Middle_East_c7_high_l_cum(end),Middle_East_mid(14),Middle_East_exp(14);
                 Pakistan_c7_high_l_cum(end),Pakistan_mid(14),Pakistan_exp(14);
                 Russia_c7_high_l_cum(end),Russia_mid(14),Russia_exp(14);
                 South_Africa_c7_high_l_cum(end),South_Africa_mid(14),South_Africa_exp(14);
                 South_America_Northern_c7_high_l_cum(end),South_America_Northern_mid(14),South_America_Northern_exp(14);
                 South_America_Southern_c7_high_l_cum(end),South_America_Southern_mid(14),South_America_Southern_exp(14);
                 South_Asia_c7_high_l_cum(end),South_Asia_mid(14),South_Asia_exp(14)
                 South_Korea_c7_high_l_cum(end),South_Korea_mid(14),South_Korea_exp(14)
                 Southeast_Asia_c7_high_l_cum(end),Southeast_Asia_mid(14),Southeast_Asia_exp(14)
                 Taiwan_c7_high_l_cum(end),Taiwan_mid(14),Taiwan_exp(14)
                 Argentina_c7_high_l_cum(end),Argentina_mid(14),Argentina_exp(14)
                 Columbia_c7_high_l_cum(end),Columbia_mid(14),Columbia_exp(14)];
owec7_global_low_mod = owec7_global_low(owec7_global_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec7_smooth_low_mod.csv', owec7_global_low_mod,'D6:F35')

owec89_global_low = [Africa_Eastern_c89_high_l_cum(end),Africa_Eastern_mid(18),Africa_Eastern_exp(18);
                 Africa_Northern_c89_high_l_cum(end),Africa_Northern_mid(18),Africa_Northern_exp(18);
                 Africa_Southern_c89_high_l_cum(end),Africa_Southern_mid(18),Africa_Southern_exp(18);
                 Africa_Western_c89_high_l_cum(end),Africa_Western_mid(18),Africa_Western_exp(18);
                 Australia_NZ_c89_high_l_cum(end),Australia_NZ_mid(18),Australia_NZ_exp(18);
                 Brazil_c89_high_l_cum(end),Brazil_mid(18),Brazil_exp(18);
                 Canada_c89_high_l_cum(end),Canada_mid(18),Canada_exp(18);
                 Central_America_and_Caribbean_c89_high_l_cum(end),Central_America_and_Caribbean_mid(18),Central_America_and_Caribbean_exp(18);
                 Central_Asia_c89_high_l_cum(end),Central_Asia_mid(18),Central_Asia_exp(18);
                 China_c89_high_l_cum(end),China_mid(18),China_exp(18);
                 EU_12_c89_high_l_cum(end),EU_12_mid(18),EU_12_exp(18);
                 EU_15_c89_high_l_cum(end),EU_15_mid(18),EU_15_exp(18);
                 Europe_Eastern_c89_high_l_cum(end),Europe_Eastern_mid(18),Europe_Eastern_exp(18);
                 Europe_Non_EU_c89_high_l_cum(end),Europe_Non_EU_mid(18),Europe_Non_EU_exp(18);
                 European_Free_Trade_Association_c89_high_l_cum(end),European_Free_Trade_Association_mid(18),European_Free_Trade_Association_exp(18);
                 India_c89_high_l_cum(end),India_mid(18),India_exp(18);
                 Indonesia_c89_high_l_cum(end),Indonesia_mid(18),Indonesia_exp(18);
                 Japan_c89_high_l_cum(end),Japan_mid(18),Japan_exp(18);
                 Mexico_c89_high_l_cum(end),Mexico_mid(18),Mexico_exp(18);
                 Middle_East_c89_high_l_cum(end),Middle_East_mid(18),Middle_East_exp(18);
                 Pakistan_c89_high_l_cum(end),Pakistan_mid(18),Pakistan_exp(18);
                 Russia_c89_high_l_cum(end),Russia_mid(18),Russia_exp(18);
                 South_Africa_c89_high_l_cum(end),South_Africa_mid(18),South_Africa_exp(18);
                 South_America_Northern_c89_high_l_cum(end),South_America_Northern_mid(18),South_America_Northern_exp(18);
                 South_America_Southern_c89_high_l_cum(end),South_America_Southern_mid(18),South_America_Southern_exp(18);
                 South_Asia_c89_high_l_cum(end),South_Asia_mid(18),South_Asia_exp(18)
                 South_Korea_c89_high_l_cum(end),South_Korea_mid(18),South_Korea_exp(18)
                 Southeast_Asia_c89_high_l_cum(end),Southeast_Asia_mid(18),Southeast_Asia_exp(18)
                 Taiwan_c89_high_l_cum(end),Taiwan_mid(18),Taiwan_exp(18)
                 Argentina_c89_high_l_cum(end),Argentina_mid(18),Argentina_exp(18)
                 Columbia_c89_high_l_cum(end),Columbia_mid(18),Columbia_exp(18)];
owec89_global_low_mod = owec89_global_low(owec89_global_low(:,1) > 0,:);
xlswrite('L210.RenewRsrcCurves_owec89_smooth_low_mod.csv', owec89_global_low_mod,'D6:F35')

capc4l_over_time = zeros(22,3);
capc4l_over_time(1:5,1) = total_capital_low(4,1);
capc4h_over_time = zeros(22,3);
capc4h_over_time(1:5,1) = total_capital_high(4,1);
capc4l_over_time(:,2) = 0.13;
capc4h_over_time(:,2) = 0.13;
capc4l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(4)));
capc4h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(4)));
capc4l_adv_over_time = capc4l_over_time;
capc4h_adv_over_time = capc4h_over_time;
for i = 6:22;
    capc4l_over_time(i,1) = capc4l_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    capc4h_over_time(i,1) = capc4h_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    capc4l_adv_over_time(i,1) = capc4l_adv_over_time(i-1,1)*0.75;
    capc4h_adv_over_time(i,1) = capc4h_adv_over_time(i-1,1)*0.75;
%     if i > 12; 
%         capc4l_over_time(i,1) = capc4l_over_time(i-1,1)*0.97; 
%         capc4h_over_time(i,1) = capc4h_over_time(i-1,1)*0.97; 
%     end
    if i > 8;
        capc4l_adv_over_time(i,1) = capc4l_adv_over_time(i-1,1)*0.95;
        capc4h_adv_over_time(i,1) = capc4h_adv_over_time(i-1,1)*0.95;
    end
end
capc4l_over_time(:,1) = capc4l_over_time(:,1)/to_gcam_dollars;
capc4h_over_time(:,1) = capc4h_over_time(:,1)/to_gcam_dollars;
capc4l_adv_over_time(:,1) = capc4l_adv_over_time(:,1)/to_gcam_dollars;
capc4h_adv_over_time(:,1) = capc4h_adv_over_time(:,1)/to_gcam_dollars;
xlswrite('L223.GlobalIntTechCapital_elec_owec4_low.csv',capc4l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec4_high.csv',capc4h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owec4_low_adv.csv',capc4l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec4_high_adv.csv',capc4h_adv_over_time,'F6:H27');      


capc5l_over_time = capc4l_over_time;
capc5h_over_time = capc4h_over_time;
capc5l_adv_over_time = capc4l_adv_over_time;
capc5h_adv_over_time = capc4h_adv_over_time;
capc5l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(5)));
capc5h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(5)));
capc5l_adv_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(5)));
capc5h_adv_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(5)));
xlswrite('L223.GlobalIntTechCapital_elec_owec5_low.csv',capc5l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec5_high.csv',capc5h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owec5_low_adv.csv',capc5l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec5_high_adv.csv',capc5h_adv_over_time,'F6:H27');      

capc6l_over_time = capc4l_over_time;
capc6h_over_time = capc4h_over_time;
capc6l_adv_over_time = capc4l_adv_over_time;
capc6h_adv_over_time = capc4h_adv_over_time;
capc6l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(6)));
capc6h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(6)));
capc6l_adv_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(6)));
capc6h_adv_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(6)));
xlswrite('L223.GlobalIntTechCapital_elec_owec6_low.csv',capc6l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec6_high.csv',capc6h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owec6_low_adv.csv',capc6l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec6_high_adv.csv',capc6h_adv_over_time,'F6:H27');      

capc7l_over_time = capc4l_over_time;
capc7h_over_time = capc4h_over_time;
capc7l_adv_over_time = capc4l_adv_over_time;
capc7h_adv_over_time = capc4h_adv_over_time;
capc7l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(7)));
capc7h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(7)));
capc7l_adv_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(7)));
capc7h_adv_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(7)));
xlswrite('L223.GlobalIntTechCapital_elec_owec7_low.csv',capc7l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec7_high.csv',capc7h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owec7_low_adv.csv',capc7l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec7_high_adv.csv',capc7h_adv_over_time,'F6:H27');      

capc89l_over_time = capc4l_over_time;
capc89h_over_time = capc4h_over_time;
capc89l_adv_over_time = capc4l_adv_over_time;
capc89h_adv_over_time = capc4h_adv_over_time;
capc89l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(8)));
capc89h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(9)));
capc89l_adv_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(8)));
capc89h_adv_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(9)));
xlswrite('L223.GlobalIntTechCapital_elec_owec89_low.csv',capc89l_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec89_high.csv',capc89h_over_time,'F6:H27');      
xlswrite('L223.GlobalIntTechCapital_elec_owec89_low_adv.csv',capc89l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalIntTechCapital_elec_owec89_high_adv.csv',capc89h_adv_over_time,'F6:H27');      

capsc4l_over_time = zeros(22,3);
capsc4l_over_time(1:5,1) = total_capital_low(4,1)+storage(5);
capsc4h_over_time = zeros(22,3);
capsc4h_over_time(1:5,1) = total_capital_high(4,1)+storage(5);
capsc4l_over_time(:,2) = 0.13;
capsc4h_over_time(:,2) = 0.13;
capsc4l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(4)));
capsc4h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(4)));
capsc4l_adv_over_time = capsc4l_over_time;
capsc4h_adv_over_time = capsc4h_over_time;
for i = 6:22;
    capsc4l_over_time(i,1) = capsc4l_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    capsc4h_over_time(i,1) = capsc4h_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    capsc4l_adv_over_time(i,1) = capsc4l_adv_over_time(i-1,1)*0.75;
    capsc4h_adv_over_time(i,1) = capsc4h_adv_over_time(i-1,1)*0.75;
%     if i > 12; 
%         capsc4l_over_time(i,1) = capsc4l_over_time(i-1,1)*0.98; 
%         capsc4h_over_time(i,1) = capsc4h_over_time(i-1,1)*0.98; 
%     end
    if i > 8;
        capsc4l_adv_over_time(i,1) = capsc4l_adv_over_time(i-1,1)*0.95;
        capsc4h_adv_over_time(i,1) = capsc4h_adv_over_time(i-1,1)*0.95;
    end
end
capsc4l_over_time(:,1) = capsc4l_over_time(:,1)/to_gcam_dollars;
capsc4h_over_time(:,1) = capsc4h_over_time(:,1)/to_gcam_dollars;
capsc4l_adv_over_time(:,1) = capsc4l_adv_over_time(:,1)/to_gcam_dollars;
capsc4h_adv_over_time(:,1) = capsc4h_adv_over_time(:,1)/to_gcam_dollars;
xlswrite('L223.GlobalTechCapital_elec_owec4_low.csv',capsc4l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec4_high.csv',capsc4h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owec4_low_adv.csv',capsc4l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec4_high_adv.csv',capsc4h_adv_over_time,'F6:H27');      

capsc5l_over_time = capsc4l_over_time;
capsc5h_over_time = capsc4h_over_time;
capsc5l_adv_over_time = capsc4l_adv_over_time;
capsc5h_adv_over_time = capsc4h_adv_over_time;
capsc5l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(5)));
capsc5h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(5)));
capsc5l_adv_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(5)));
capsc5h_adv_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(5)));
xlswrite('L223.GlobalTechCapital_elec_owec5_low.csv',capsc5l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec5_high.csv',capsc5h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owec5_low_adv.csv',capsc5l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec5_high_adv.csv',capsc5h_adv_over_time,'F6:H27');      

capsc6l_over_time = capsc4l_over_time;
capsc6h_over_time = capsc4h_over_time;
capsc6l_adv_over_time = capsc4l_adv_over_time;
capsc6h_adv_over_time = capsc4h_adv_over_time;
capsc6l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(6)));
capsc6h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(6)));
capsc6l_adv_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(6)));
capsc6h_adv_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(6)));
xlswrite('L223.GlobalTechCapital_elec_owec6_low.csv',capsc6l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec6_high.csv',capsc6h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owec6_low_adv.csv',capsc6l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec6_high_adv.csv',capsc6h_adv_over_time,'F6:H27');      

capsc7l_over_time = capsc4l_over_time;
capsc7h_over_time = capsc4h_over_time;
capsc7l_adv_over_time = capsc4l_adv_over_time;
capsc7h_adv_over_time = capsc4h_adv_over_time;
capsc7l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(7)));
capsc7h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(7)));
capsc7l_adv_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(7)));
capsc7h_adv_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(7)));
xlswrite('L223.GlobalTechCapital_elec_owec7_low.csv',capsc7l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec7_high.csv',capsc7h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owec7_low_adv.csv',capsc7l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec7_high_adv.csv',capsc7h_adv_over_time,'F6:H27');      

capsc89l_over_time = capsc4l_over_time;
capsc89h_over_time = capsc4h_over_time;
capsc89l_adv_over_time = capsc4l_adv_over_time;
capsc89h_adv_over_time = capsc4h_adv_over_time;
capsc89l_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(8)));
capsc89h_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(9)));
capsc89l_adv_over_time(:,3) = cf_high(find(wind_speeds == global_bins_high(8)));
capsc89h_adv_over_time(:,3) = cf_low(find(wind_speeds == global_bins_low(9)));
xlswrite('L223.GlobalTechCapital_elec_owec89_low.csv',capsc89l_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec89_high.csv',capsc89h_over_time,'F6:H27');      
xlswrite('L223.GlobalTechCapital_elec_owec89_low_adv.csv',capsc89l_adv_over_time,'F6:H27');  
xlswrite('L223.GlobalTechCapital_elec_owec89_high_adv.csv',capsc89h_adv_over_time,'F6:H27');      

omc4l_over_time = zeros(22,2);
omc4l_over_time(1:5,1) = om_low;
omc4h_over_time = zeros(22,2);
omc4h_over_time(1:5,1) = om_high;
omc4l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(4)));
omc4h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(4)));
omc4l_adv_over_time = omc4l_over_time;
omc4h_adv_over_time = omc4h_over_time;
for i = 6:22;
    omc4l_over_time(i,1) = omc4l_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    omc4h_over_time(i,1) = omc4h_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    omc4l_adv_over_time(i,1) = omc4l_adv_over_time(i-1,1)*0.75;
    omc4h_adv_over_time(i,1) = omc4h_adv_over_time(i-1,1)*0.75;
%     if i > 12; 
%         omc4l_over_time(i,1) = omc4l_over_time(i-1,1)*0.97; 
%         omc4h_over_time(i,1) = omc4h_over_time(i-1,1)*0.97; 
%     end
    if i > 8;
        omc4l_adv_over_time(i,1) = omc4l_adv_over_time(i-1,1)*0.95;
        omc4h_adv_over_time(i,1) = omc4h_adv_over_time(i-1,1)*0.95;
    end
end
omc4l_over_time(:,1) = omc4l_over_time(:,1)/to_gcam_dollars;
omc4h_over_time(:,1) = omc4h_over_time(:,1)/to_gcam_dollars;
omc4l_adv_over_time(:,1) = omc4l_adv_over_time(:,1)/to_gcam_dollars;
omc4h_adv_over_time(:,1) = omc4h_adv_over_time(:,1)/to_gcam_dollars;
xlswrite('L223.GlobalIntTechOMfixed_elec_owec4_low.csv',omc4l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec4_high.csv',omc4h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owec4_low_adv.csv',omc4l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec4_high_adv.csv',omc4h_adv_over_time,'F6:G27');      

omc5l_over_time = omc4l_over_time;
omc5h_over_time = omc4h_over_time;
omc5l_adv_over_time = omc4l_adv_over_time;
omc5h_adv_over_time = omc4h_adv_over_time;
omc5l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(5)));
omc5h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(5)));
omc5l_adv_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(5)));
omc5h_adv_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(5)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owec5_low.csv',omc5l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec5_high.csv',omc5h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owec5_low_adv.csv',omc5l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec5_high_adv.csv',omc5h_adv_over_time,'F6:G27');      

omc6l_over_time = omc4l_over_time;
omc6h_over_time = omc4h_over_time;
omc6l_adv_over_time = omc4l_adv_over_time;
omc6h_adv_over_time = omc4h_adv_over_time;
omc6l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(6)));
omc6h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(6)));
omc6l_adv_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(6)));
omc6h_adv_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(6)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owec6_low.csv',omc6l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec6_high.csv',omc6h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owec6_low_adv.csv',omc6l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec6_high_adv.csv',omc6h_adv_over_time,'F6:G27');      

omc7l_over_time = omc4l_over_time;
omc7h_over_time = omc4h_over_time;
omc7l_adv_over_time = omc4l_adv_over_time;
omc7h_adv_over_time = omc4h_adv_over_time;
omc7l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(7)));
omc7h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(7)));
omc7l_adv_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(7)));
omc7h_adv_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(7)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owec7_low.csv',omc7l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec7_high.csv',omc7h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owec7_low_adv.csv',omc7l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec7_high_adv.csv',omc7h_adv_over_time,'F6:G27');      

omc89l_over_time = omc4l_over_time;
omc89h_over_time = omc4h_over_time;
omc89l_adv_over_time = omc4l_adv_over_time;
omc89h_adv_over_time = omc4h_adv_over_time;
omc89l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(8)));
omc89h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(9)));
omc89l_adv_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(8)));
omc89h_adv_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(9)));
xlswrite('L223.GlobalIntTechOMfixed_elec_owec89_low.csv',omc89l_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec89_high.csv',omc89h_over_time,'F6:G27');      
xlswrite('L223.GlobalIntTechOMfixed_elec_owec89_low_adv.csv',omc89l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalIntTechOMfixed_elec_owec89_high_adv.csv',omc89h_adv_over_time,'F6:G27');      

omsc4l_over_time = zeros(22,2);
omsc4l_over_time(1:5,1) = om_low+omstorage(5);
omsc4h_over_time = zeros(22,2);
omsc4h_over_time(1:5,1) = om_high+omstorage(5);
omsc4l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(4)));
omsc4h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(4)));
omsc4l_adv_over_time = omsc4l_over_time;
omsc4h_adv_over_time = omsc4h_over_time;
for i = 6:22;
    omsc4l_over_time(i,1) = omsc4l_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    omsc4h_over_time(i,1) = omsc4h_over_time(i-1,1)*onshore_cap(i,1)/onshore_cap(i-1,1);
    omsc4l_adv_over_time(i,1) = omsc4l_adv_over_time(i-1,1)*0.75;
    omsc4h_adv_over_time(i,1) = omsc4h_adv_over_time(i-1,1)*0.75;
%     if i > 12; 
%         omsc4l_over_time(i,1) = omsc4l_over_time(i-1,1)*0.97; 
%         omsc4h_over_time(i,1) = omsc4h_over_time(i-1,1)*0.97; 
%     end
    if i > 8;
        omsc4l_adv_over_time(i,1) = omsc4l_adv_over_time(i-1,1)*0.95;
        omsc4h_adv_over_time(i,1) = omsc4h_adv_over_time(i-1,1)*0.95;
    end
end
omsc4l_over_time(:,1) = omsc4l_over_time(:,1)/to_gcam_dollars;
omsc4h_over_time(:,1) = omsc4h_over_time(:,1)/to_gcam_dollars;
omsc4l_adv_over_time(:,1) = omsc4l_adv_over_time(:,1)/to_gcam_dollars;
omsc4h_adv_over_time(:,1) = omsc4h_adv_over_time(:,1)/to_gcam_dollars;
xlswrite('L223.GlobalTechOMfixed_elec_owec4_low.csv',omsc4l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec4_high.csv',omsc4h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owec4_low_adv.csv',omsc4l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec4_high_adv.csv',omsc4h_adv_over_time,'F6:G27');      

omsc5l_over_time = omsc4l_over_time;
omsc5h_over_time = omsc4h_over_time;
omsc5l_adv_over_time = omsc4l_adv_over_time;
omsc5h_adv_over_time = omsc4h_adv_over_time;
omsc5l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(5)));
omsc5h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(5)));
omsc5l_adv_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(5)));
omsc5h_adv_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(5)));
xlswrite('L223.GlobalTechOMfixed_elec_owec5_low.csv',omsc5l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec5_high.csv',omsc5h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owec5_low_adv.csv',omsc5l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec5_high_adv.csv',omsc5h_adv_over_time,'F6:G27');      

omsc6l_over_time = omsc4l_over_time;
omsc6h_over_time = omsc4h_over_time;
omsc6l_adv_over_time = omsc4l_adv_over_time;
omsc6h_adv_over_time = omsc4h_adv_over_time;
omsc6l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(6)));
omsc6h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(6)));
omsc6l_adv_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(6)));
omsc6h_adv_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(6)));
xlswrite('L223.GlobalTechOMfixed_elec_owec6_low.csv',omsc6l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec6_high.csv',omsc6h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owec6_low_adv.csv',omsc6l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec6_high_adv.csv',omsc6h_adv_over_time,'F6:G27');      

omsc7l_over_time = omsc4l_over_time;
omsc7h_over_time = omsc4h_over_time;
omsc7l_adv_over_time = omsc4l_adv_over_time;
omsc7h_adv_over_time = omsc4h_adv_over_time;
omsc7l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(7)));
omsc7h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(7)));
omsc7l_adv_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(7)));
omsc7h_adv_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(7)));
xlswrite('L223.GlobalTechOMfixed_elec_owec7_low.csv',omsc7l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec7_high.csv',omsc7h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owec7_low_adv.csv',omsc7l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec7_high_adv.csv',omsc7h_adv_over_time,'F6:G27');      

omsc89l_over_time = omsc4l_over_time;
omsc89h_over_time = omsc4h_over_time;
omsc89l_adv_over_time = omsc4l_adv_over_time;
omsc89h_adv_over_time = omsc4h_adv_over_time;
omsc89l_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(8)));
omsc89h_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(9)));
omsc89l_adv_over_time(:,2) = cf_high(find(wind_speeds == global_bins_high(8)));
omsc89h_adv_over_time(:,2) = cf_low(find(wind_speeds == global_bins_low(9)));
xlswrite('L223.GlobalTechOMfixed_elec_owec89_low.csv',omsc89l_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec89_high.csv',omsc89h_over_time,'F6:G27');      
xlswrite('L223.GlobalTechOMfixed_elec_owec89_low_adv.csv',omsc89l_adv_over_time,'F6:G27');  
xlswrite('L223.GlobalTechOMfixed_elec_owec89_high_adv.csv',omsc89h_adv_over_time,'F6:G27');      

cf_c4_low = zeros(30*22*2); cf_c4_high = cf_c4_low;
cf_c4_high(:,1) = cf_low(find(wind_speeds == global_bins_low(4)));
cf_c4_low(:,1) = cf_high(find(wind_speeds == global_bins_high(4)));
xlswrite('L223.StubTechCapFactor_elec_owec4_low_mod.csv',cf_c4_low,'G6:G1325')
xlswrite('L223.StubTechCapFactor_elec_owec4_high_mod.csv',cf_c4_high,'G6:G1325')
xlswrite('L223.StubTechCapFactor_elec_owec4_low_mod.csv',cf_c4_low,'I6:I1325')
xlswrite('L223.StubTechCapFactor_elec_owec4_high_mod.csv',cf_c4_high,'I6:I1325')

cf_c5_low = zeros(28*22*2); cf_c5_high = cf_c5_low;
cf_c5_high(:,1) = cf_low(find(wind_speeds == global_bins_low(5)));
cf_c5_low(:,1) = cf_high(find(wind_speeds == global_bins_high(5)));
xlswrite('L223.StubTechCapFactor_elec_owec5_low_mod.csv',cf_c5_low,'G6:G1237')
xlswrite('L223.StubTechCapFactor_elec_owec5_high_mod.csv',cf_c5_high,'G6:G1237')
xlswrite('L223.StubTechCapFactor_elec_owec5_low_mod.csv',cf_c5_low,'I6:I1237')
xlswrite('L223.StubTechCapFactor_elec_owec5_high_mod.csv',cf_c5_high,'I6:I1237')

cf_c6_low = zeros(25*22*2); cf_c6_high = cf_c6_low;
cf_c6_high(:,1) = cf_low(find(wind_speeds == global_bins_low(6)));
cf_c6_low(:,1) = cf_high(find(wind_speeds == global_bins_high(6)));
xlswrite('L223.StubTechCapFactor_elec_owec6_low_mod.csv',cf_c6_low,'G6:G1105')
xlswrite('L223.StubTechCapFactor_elec_owec6_high_mod.csv',cf_c6_high,'G6:G1105')
xlswrite('L223.StubTechCapFactor_elec_owec6_low_mod.csv',cf_c6_low,'I6:I1105')
xlswrite('L223.StubTechCapFactor_elec_owec6_high_mod.csv',cf_c6_high,'I6:I1105')

cf_c7_low = zeros(18*22*2); cf_c7_high = cf_c7_low;
cf_c7_high(:,1) = cf_low(find(wind_speeds == global_bins_low(7)));
cf_c7_low(:,1) = cf_high(find(wind_speeds == global_bins_high(7)));
xlswrite('L223.StubTechCapFactor_elec_owec7_low_mod.csv',cf_c7_low,'G6:G797')
xlswrite('L223.StubTechCapFactor_elec_owec7_high_mod.csv',cf_c7_high,'G6:G797')
xlswrite('L223.StubTechCapFactor_elec_owec7_low_mod.csv',cf_c7_low,'I6:I797')
xlswrite('L223.StubTechCapFactor_elec_owec7_high_mod.csv',cf_c7_high,'I6:I797')

cf_c89_low = zeros(17*22*2); cf_c89_high = cf_c89_low;
cf_c89_high(:,1) = cf_low(find(wind_speeds == global_bins_low(8)));
cf_c89_low(:,1) = cf_high(find(wind_speeds == global_bins_high(8)));
xlswrite('L223.StubTechCapFactor_elec_owec89_low_mod.csv',cf_c89_low,'G6:G753')
xlswrite('L223.StubTechCapFactor_elec_owec89_high_mod.csv',cf_c89_high,'G6:G753')
xlswrite('L223.StubTechCapFactor_elec_owec89_low_mod.csv',cf_c89_low,'I6:I753')
xlswrite('L223.StubTechCapFactor_elec_owec89_high_mod.csv',cf_c89_high,'I6:I753')
