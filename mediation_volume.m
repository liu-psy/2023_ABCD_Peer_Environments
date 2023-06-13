% mediation analysis -----------------------------------------------------------
% peer environment of ABCD -----------------------------------------------------
% Brain strcutre - Behaviors ---------------------------------------------------
clear all; clc;
cd H:/ABCD/Relsease4.0/Package_1194636/results/peer_environments
% load('mediation_brain_behavior_p/mediation_volume_results.mat')

% load data
mediations_volumes = readtable('mediation_volume.csv');
vars = mediations_volumes.Properties.VariableNames(2:99)';
mediations = table2array(mediations_volumes(:,2:99));

% mediation --------------------------------------------------------------------
% PFI
PFI_volume = PFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_total')));
PFI_ifpllh = PFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_ifpllh')));
PFI_paracnlh = PFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_paracnlh')));
PFI_precnlh = PFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_precnlh')));
PFI_rracatelh = PFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_rracatelh')));
PFI_sufrlh = PFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_sufrlh')));
PFI_postcnrh = PFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_postcnrh')));
PFI_insularh = PFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_insularh')));

% DFI
DFI_volume = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_total')));
DFI_fusiformlh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_fusiformlh')));
DFI_locclh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_locclh')));
DFI_lobfrlh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_lobfrlh')));
DFI_supllh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_supllh')));
DFI_iftmrh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_iftmrh')));
DFI_loccrh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_loccrh')));
DFI_mdtmrh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_mdtmrh')));
DFI_ptcaterh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_ptcaterh')));
DFI_precnrh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_precnrh')));
DFI_pcrh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_pcrh')));
DFI_suplrh = DFI_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_suplrh')));

% PII
PII_volume = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_total')));
PII_ifpllh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_ifpllh')));
PII_locclh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_locclh')));
PII_lobfrlh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_lobfrlh')));
PII_mdtmlh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_mdtmlh')));
PII_paracnlh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_paracnlh')));
PII_postcnlh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_fusiformlh')));
PII_precnlh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_postcnlh')));
PII_pclh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_pclh')));
PII_rracatelh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_rracatelh')));
PII_sufrlh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_sufrlh')));
PII_insulalh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_insulalh')));
PII_banksstsrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_banksstsrh')));
PII_cuneusrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_fusiformlh')));
PII_fusiformrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_fusiformrh')));
PII_ifplrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_ifplrh')));
PII_mdtmrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_mdtmrh')));
PII_paracnrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_paracnrh')));
PII_postcnrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_postcnrh')));
PII_ptcaterh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_ptcaterh')));
PII_precnrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_precnrh')));
PII_sufrrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_sufrrh')));
PII_suplrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_suplrh')));
PII_smrh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_smrh')));
PII_insularh = PII_volumes(mediations, find(strcmp(vars, 'smri_vol_cdk_insularh')));

% output files ------------------------------------------------------------
cd H:/ABCD/Relsease4.0/Package_1194636/results/peer_environments/mediation_brain_behavior_p

% save results
save mediation_volume_results.mat

% output files
% cd H:/ABCD/Relsease4.0/Package_1194636/results/peer_environments/mediation
csvwrite('PFI_volume.csv', PFI_volume)
csvwrite('PFI_ifpllh.csv', PFI_ifpllh)
csvwrite('PFI_paracnlh.csv', PFI_paracnlh)
csvwrite('PFI_precnlh.csv', PFI_precnlh)
csvwrite('PFI_rracatelh.csv', PFI_rracatelh)
csvwrite('PFI_sufrlh.csv', PFI_sufrlh)
csvwrite('PFI_postcnrh.csv', PFI_postcnrh)
csvwrite('PFI_insularh.csv', PFI_insularh)

csvwrite('DFI_volume.csv', DFI_volume)
csvwrite('DFI_fusiformlh.csv', DFI_fusiformlh)
csvwrite('DFI_locclh.csv', DFI_locclh)
csvwrite('DFI_lobfrlh.csv', DFI_lobfrlh)
csvwrite('DFI_supllh.csv', DFI_supllh)
csvwrite('DFI_iftmrh.csv', DFI_iftmrh)
csvwrite('DFI_loccrh.csv', DFI_loccrh)
csvwrite('DFI_mdtmrh.csv', DFI_mdtmrh)
csvwrite('DFI_ptcaterh.csv', DFI_ptcaterh)
csvwrite('DFI_precnrh.csv', DFI_precnrh)
csvwrite('DFI_pcrh.csv', DFI_pcrh)
csvwrite('DFI_suplrh.csv', DFI_suplrh)

csvwrite('PII_volume.csv', PII_volume)
csvwrite('PII_ifpllh.csv', PII_ifpllh)
csvwrite('PII_locclh.csv', PII_locclh)
csvwrite('PII_lobfrlh.csv', PII_lobfrlh)
csvwrite('PII_mdtmlh.csv', PII_mdtmlh)
csvwrite('PII_paracnlh.csv', PII_paracnlh)
csvwrite('PII_postcnlh.csv', PII_postcnlh)
csvwrite('PII_precnlh.csv', PII_precnlh)
csvwrite('PII_pclh.csv', PII_pclh)
csvwrite('PII_rracatelh.csv', PII_rracatelh)
csvwrite('PII_sufrlh.csv', PII_sufrlh)
csvwrite('PII_insulalh.csv', PII_insulalh)
csvwrite('PII_banksstsrh.csv', PII_banksstsrh)
csvwrite('PII_cuneusrh.csv', PII_cuneusrh)
csvwrite('PII_fusiformrh.csv', PII_fusiformrh)
csvwrite('PII_ifplrh.csv', PII_ifplrh)
csvwrite('PII_mdtmrh.csv', PII_mdtmrh)
csvwrite('PII_paracnrh.csv', PII_paracnrh)
csvwrite('PII_postcnrh.csv', PII_postcnrh)
csvwrite('PII_ptcaterh.csv', PII_ptcaterh)
csvwrite('PII_precnrh.csv', PII_precnrh)
csvwrite('PII_sufrrh.csv', PII_sufrrh)
csvwrite('PII_suplrh.csv', PII_suplrh)
csvwrite('PII_smrh.csv', PII_smrh)
csvwrite('PII_insularh.csv', PII_insularh)