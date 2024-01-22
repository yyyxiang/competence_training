function run_bms(filename, exp)

%   Uses the bms.m script to calculate protected exceedance probabilities
%   from model evidence for each participant (approximated as -0.5*BIC).
%   Example input: filename = 'exp1_model_evidence.csv'; exp = 1;

    model_evidence = readmatrix(filename);
    [~,~,~,pxp,~,~] = bms(model_evidence);
    pxp = array2table(pxp, 'VariableNames', {'planning', 'exploitation', 'equity', 'learning', 'equality'});
    writetable(pxp, ['exp', num2str(exp), '_protected_exceedance_probabilities.csv'], 'WriteVariableNames', true)
    