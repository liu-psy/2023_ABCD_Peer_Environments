% mediation analysis -----------------------------------------------------------
% peer environment of ABCD -----------------------------------------------------
% RSFCs - Behaviors ------------------------------------------------------------
clear all; clc;

cd H:/ABCD/Relsease4.0/Package_1194636/results/peer_environments
% load('mediation_brain_behavior_p/meadiations_rsfcs_results.mat')

% load data --------------------------------------------------------------------
mediations_rsfcs = readtable('mediation_rsfc.csv');
vars = mediations_rsfcs.Properties.VariableNames(2:137)';
mediations = table2array(mediations_rsfcs(:,2:137));

% PFI - cortical RSFCs
PFI_ad_ca = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ad_ngd_ca')));
PFI_ad_smm = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ad_ngd_smm')));    % both
PFI_ad_sa = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ad_ngd_sa')));
PFI_cgc_dt = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_cgc_ngd_dt')));    % both
PFI_cgc_dla = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_cgc_ngd_dla')));
PFI_ca_smh = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ca_ngd_smh')));
PFI_ca_smm = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ca_ngd_smm')));
PFI_ca_vta = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ca_ngd_vta')));
PFI_ca_vs = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ca_ngd_vs')));
PFI_dt_dla = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_dla')));    % both
PFI_dt_fo = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_fo')));
PFI_dt_rspltp = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_rspltp')));
PFI_dt_smm = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_smm')));
PFI_rspltp_sa = PFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_rspltp_ngd_sa')));

% DFI - cortical RSFCs
DFI_ad_ad = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ad_ngd_ad')));
DFI_dt_dt = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_dt')));
DFI_dla_dla = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dla_ngd_dla')));
DFI_smh_smh = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_smh_ngd_smh')));
DFI_vs_vs = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_vs_ngd_vs')));
DFI_ad_smh = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ad_ngd_smh')));
DFI_ad_smm = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ad_ngd_smm')));
DFI_cgc_ca = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_cgc_ngd_ca')));
DFI_cgc_dt = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_cgc_ngd_dt')));
DFI_cgc_vs = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_cgc_ngd_vs')));
DFI_dt_dla = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_dla')));
DFI_dt_vs = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_vs')));
DFI_dla_vta = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dla_ngd_vta')));
DFI_smh_smm = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_smh_ngd_smm')));

% DFI - cortico-subcortical RSFCs
DFI_au_cde = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_au_scs_cde')));
DFI_au_hp = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_au_scs_hp')));
DFI_au_aa = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_au_scs_aa')));
DFI_cerc_thp = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_cerc_scs_thp')));
DFI_cerc_pt = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_cerc_scs_pt')));
DFI_cerc_hp = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_cerc_scs_hp')));
DFI_cerc_ag = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_cerc_scs_ag')));
DFI_copa_crcx = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_copa_scs_crcx')));
DFI_copa_cde = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_copa_scs_cde')));
DFI_copa_vtdc = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_copa_scs_vtdc')));
DFI_df_pl = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_df_scs_pl')));
DFI_df_ag = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_df_scs_ag')));
DFI_df_aa = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_df_scs_aa')));
DFI_dsa_hp = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_dsa_scs_hp')));
DFI_dsa_ag = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_dsa_scs_ag')));
DFI_dsa_vtdc = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_dsa_scs_vtdc')));
DFI_fopa_crcx = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_fopa_scs_crcx')));
DFI_fopa_thp = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_fopa_scs_thp')));
DFI_fopa_cde = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_fopa_scs_cde')));
DFI_fopa_ag = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_fopa_scs_ag')));
DFI_rst_aa = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_rst_scs_aa')));
DFI_smh_crcx = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_smh_scs_crcx')));
DFI_smh_cde = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_smh_scs_cde')));
DFI_smh_pt = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_smh_scs_pt')));
DFI_smh_pl = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_smh_scs_pl')));
DFI_smh_aa = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_smh_scs_aa')));
DFI_smm_pl = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_smm_scs_pl')));
DFI_smm_hp = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_smm_scs_hp')));
DFI_smm_ag = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_smm_scs_ag')));
DFI_sa_hp = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_sa_scs_hp')));
DFI_sa_aa = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_sa_scs_aa')));
DFI_sa_vtdc = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_sa_scs_vtdc')));
DFI_vta_cde = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_vta_scs_cde')));
DFI_vta_pt = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_vta_scs_pt')));
DFI_vta_pl = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_vta_scs_pl')));
DFI_vta_hp = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_vta_scs_hp')));
DFI_vs_vtdc = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_vs_scs_vtdc')));
DFI_cerc_bs = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_cerc_scs_bs')));
DFI_dsa_bs = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_dsa_scs_bs')));
DFI_rst_bs = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_rst_scs_bs')));
DFI_smh_bs = DFI_rsfcs(mediations, find(strcmp(vars, 'rsfmri_cor_ngd_smh_scs_bs')));

% PII cortical RSFCs
PII_cgc_cgc = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_cgc_ngd_cgc')));
PII_dt_dt = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_dt')));
PII_dla_dla = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dla_ngd_dla')));
PII_ad_ca = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ad_ngd_ca')));
PII_cgc_dt = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_cgc_ngd_dt')));
PII_cgc_dla = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_cgc_ngd_dla')));
PII_cgc_vs = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_cgc_ngd_vs')));
PII_ca_smh = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ca_ngd_smh')));
PII_ca_vta = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_ca_ngd_vta')));
PII_dt_dla = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_dla')));
PII_dt_fo = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_fo')));
PII_dt_rspltp = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dt_ngd_rspltp')));
PII_dla_vs = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_dla_ngd_vs')));
PII_rspltp_sa = PII_rsfcs(mediations, find(strcmp(vars, 'rsfmri_c_ngd_rspltp_ngd_sa')));

% output ------------------------------------------------------------------
cd H:/ABCD/Relsease4.0/Package_1194636/results/peer_environments/mediation_brain_behavior_p

% save results
save meadiations_rsfcs_results.mat

% PFI
csvwrite('PFI_ad_ca.csv', PFI_ad_ca)
csvwrite('PFI_ad_smm.csv', PFI_ad_smm)
csvwrite('PFI_ad_sa.csv', PFI_ad_sa)
csvwrite('PFI_cgc_dt.csv', PFI_cgc_dt)
csvwrite('PFI_cgc_dla.csv', PFI_cgc_dla)
csvwrite('PFI_ca_smh.csv', PFI_ca_smh)
csvwrite('PFI_ca_smm.csv', PFI_ca_smm)
csvwrite('PFI_ca_vta.csv', PFI_ca_vta)
csvwrite('PFI_ca_vs.csv', PFI_ca_vs)
csvwrite('PFI_dt_dla.csv', PFI_dt_dla)
csvwrite('PFI_dt_fo.csv', PFI_dt_fo)
csvwrite('PFI_dt_rspltp.csv', PFI_dt_rspltp)
csvwrite('PFI_dt_smm.csv', PFI_dt_smm)
csvwrite('PFI_rspltp_sa.csv', PFI_rspltp_sa)

% DFI
csvwrite('DFI_ad_ad.csv', DFI_ad_ad)
csvwrite('DFI_dt_dt.csv', DFI_dt_dt)
csvwrite('DFI_dla_dla.csv', DFI_dla_dla)
csvwrite('DFI_smh_smh.csv', DFI_smh_smh)
csvwrite('DFI_vs_vs.csv', DFI_vs_vs)
csvwrite('DFI_ad_smh.csv', DFI_ad_smh)
csvwrite('DFI_ad_smm.csv', DFI_ad_smm)   %
csvwrite('DFI_cgc_ca.csv', DFI_cgc_ca)
csvwrite('DFI_cgc_dt.csv', DFI_cgc_dt)   %
csvwrite('DFI_cgc_vs.csv', DFI_cgc_vs)
csvwrite('DFI_dt_dla.csv', DFI_dt_dla)   %
csvwrite('DFI_dt_vs.csv', DFI_dt_vs)
csvwrite('DFI_dla_vta.csv', DFI_dla_vta)
csvwrite('DFI_smh_smm.csv', DFI_smh_smm)
% DFI cortico-subcortical FCs
csvwrite('DFI_au_cde.csv', DFI_au_cde)
csvwrite('DFI_au_hp.csv', DFI_au_hp)
csvwrite('DFI_au_aa.csv', DFI_au_aa)
csvwrite('DFI_cerc_thp.csv', DFI_cerc_thp)
csvwrite('DFI_cerc_pt.csv', DFI_cerc_pt)
csvwrite('DFI_cerc_hp.csv', DFI_cerc_hp)
csvwrite('DFI_cerc_ag.csv', DFI_cerc_ag)
csvwrite('DFI_copa_crcx.csv', DFI_copa_crcx)
csvwrite('DFI_copa_cde.csv', DFI_copa_cde)
csvwrite('DFI_copa_vtdc.csv', DFI_copa_vtdc)
csvwrite('DFI_df_pl.csv', DFI_df_pl)
csvwrite('DFI_df_ag.csv', DFI_dt_vs)
csvwrite('DFI_df_aa.csv', DFI_df_aa)
csvwrite('DFI_dsa_hp.csv', DFI_dsa_hp)
csvwrite('DFI_dsa_ag.csv', DFI_dsa_ag)
csvwrite('DFI_dsa_vtdc.csv', DFI_dsa_vtdc)
csvwrite('DFI_fopa_crcx.csv', DFI_fopa_crcx)
csvwrite('DFI_fopa_thp.csv', DFI_fopa_thp)
csvwrite('DFI_fopa_cde.csv', DFI_fopa_cde)
csvwrite('DFI_fopa_ag.csv', DFI_fopa_ag)
csvwrite('DFI_rst_aa.csv', DFI_rst_aa)
csvwrite('DFI_smh_crcx.csv', DFI_smh_crcx)
csvwrite('DFI_smh_cde.csv', DFI_smh_cde)
csvwrite('DFI_smh_pt.csv', DFI_smh_pt)
csvwrite('DFI_smh_pl.csv', DFI_smh_pl)
csvwrite('DFI_smh_aa.csv', DFI_smh_aa)
csvwrite('DFI_smm_pl.csv', DFI_smm_pl)
csvwrite('DFI_smm_hp.csv', DFI_smm_hp)
csvwrite('DFI_smm_ag.csv', DFI_smm_ag)
csvwrite('DFI_sa_hp.csv', DFI_sa_hp)
csvwrite('DFI_sa_aa.csv', DFI_sa_aa)
csvwrite('DFI_sa_vtdc.csv', DFI_sa_vtdc)
csvwrite('DFI_vta_cde.csv', DFI_vta_cde)
csvwrite('DFI_vta_pt.csv', DFI_vta_pt)
csvwrite('DFI_vta_pl.csv', DFI_vta_pl)
csvwrite('DFI_vta_hp.csv', DFI_vta_hp)
csvwrite('DFI_vs_vtdc.csv', DFI_vs_vtdc)
csvwrite('DFI_cerc_bs.csv', DFI_cerc_bs)
csvwrite('DFI_dsa_bs.csv', DFI_dsa_bs)
csvwrite('DFI_rst_bs.csv', DFI_rst_bs)
csvwrite('DFI_smh_bs.csv', DFI_smh_bs)

% PII
csvwrite('PII_cgc_cgc.csv', PII_cgc_cgc)
csvwrite('PII_dt_dt.csv', PII_dt_dt)
csvwrite('PII_dla_dla.csv', PII_dla_dla)
csvwrite('PII_ad_ca.csv', PII_ad_ca)
csvwrite('PII_cgc_dt.csv', PII_cgc_dt)
csvwrite('PII_cgc_dla.csv', PII_cgc_dla)
csvwrite('PII_cgc_vs.csv', PII_cgc_vs)
csvwrite('PII_ca_smh.csv', PII_ca_smh)
csvwrite('PII_ca_vta.csv', PII_ca_vta)
csvwrite('PII_dt_dla.csv', PII_dt_dla)
csvwrite('PII_dt_fo.csv', PII_dt_fo)
csvwrite('PII_dt_rspltp.csv', PII_dt_rspltp)
csvwrite('PII_dla_vs.csv', PII_dla_vs)
csvwrite('PII_rspltp_sa.csv', PII_rspltp_sa)