% Spatial correlations between t-maps of brain structure-peer environments associations and neurotransmitter density
% Toolbox: JuSpace

clear all; clc;

cd 'H:\ABCD\Relsease4.0\Package_1194636\results\peer_environments\pet\tmaps'
% create t maps
ref_num = [1001;1002;1003;1004;1006;1007;1008;1009;1010;1011;1012;1013;1014;1015;1016;1017;1018;...
    1019;1020;1021;1022;1023;1024;1025;1026;1027;1028;1029;1030;1031;1032;1033;1034;1035;...
    2001;2002;2003;2004;2006;2007;2008;2009;2010;2011;2012;2013;2014;2015;2016;2017;2018;...
    2019;2020;2021;2022;2023;2024;2025;2026;2027;2028;2029;2030;2031;2032;2033;2034;2035];

APARC_atlas = 'aparc+aseg.min152.nii';

% load t maps from LMM models
tmap_vol_delinquent = importdata('tmap_vol_delinquent.txt');
tmap_vol_prosocial = importdata('tmap_vol_prosocial.txt');
tmap_vol_profriends = importdata('tmap_vol_profriends.txt');
tmap_thick_delinquent = importdata('tmap_thick_delinquent.txt');
tmap_thick_prosocial = importdata('tmap_thick_prosocial.txt');
tmap_thick_profriends = importdata('tmap_thick_profriends.txt');
tmap_area_delinquent = importdata('tmap_area_delinquent.txt');
tmap_area_prosocial = importdata('tmap_area_prosocial.txt');
tmap_area_profriends = importdata('tmap_area_profriends.txt');

% ROT to Nifti
ROI2nifti(tmap_vol_delinquent, APARC_atlas, ref_num, 'vol_delinquent');
ROI2nifti(tmap_vol_prosocial, APARC_atlas, ref_num, 'vol_prosocial');
ROI2nifti(tmap_vol_profriends, APARC_atlas, ref_num, 'vol_profriends');
ROI2nifti(tmap_thick_delinquent, APARC_atlas, ref_num, 'thick_delinquent');
ROI2nifti(tmap_thick_prosocial, APARC_atlas, ref_num, 'thick_prosocial');
ROI2nifti(tmap_thick_profriends, APARC_atlas, ref_num, 'thick_profriends');
ROI2nifti(tmap_area_delinquent, APARC_atlas, ref_num, 'area_delinquent');
ROI2nifti(tmap_area_prosocial, APARC_atlas, ref_num, 'area_prosocial');
ROI2nifti(tmap_area_profriends, APARC_atlas, ref_num, 'area_profriends');

% command for JuSpace -----------------------------------------------------
group1 = {
    'area_delinquent.nii',
    'thick_delinquent.nii',
    'vol_delinquent.nii',
    'area_profriends.nii',
    'thick_profriends.nii',
    'vol_profriends.nii',
    'area_prosocial.nii',
    'thick_prosocial.nii',
    'vol_prosocial.nii'
    };
group2 = {};
PET = {
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\5HT1a_WAY_HC36.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\5HT1b_P943_HC22.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\5HT2a_ALT_HC19.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\SERT_DASB_HC30.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\D1_SCH23390_c11.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\D2_RACLOPRIDE_c11.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\DAT_DATSPECT.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\FDOPA_f18.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\GABAa_FLUMAZENIL_c11.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\NAT_MRB_c11.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\VAChT_feobv_hc18_aghourian.nii',
        'E:\Matlab\Packages\JuSpace\JuSpace_v1.4\PETatlas\mGluR5_abp_hc73_smart.nii'
        };
% set atlas (default)
ATLAS = ['E:/Matlab/Packages/JuSpace/JuSpace_v1.4/atlas/m_labels_Neuromorphometrics.nii'];
% each file in list 1
OPTIONS = [4; 1; 0; 0; 1];
% permutaion
Nperm = 1000;

% compute
[res,p_all,stats,data,D1,D2,data_PET,Resh,T1] = compute_DomainGauges(group1, group2, PET, ATLAS, OPTIONS, 'PET_result');
% compute p_exact
[p_exact,dist_rand] = compute_exact_spatial_pvalue(D1,data_PET, ATLAS, res, Nperm, OPTIONS, PET, T1);
% results (add p_exact)
Resh(:,end+1) = [{'p_exact'}; num2cell_my(p_exact')];

cd H:\ABCD\Relsease4.0\Package_1194636\results\peer_environments\pet
% output results
writecell(Resh, 'Resh.csv')
% save results
save PET_List1_Spearman_withExactSpatialP.mat ...
    Resh Nperm stats D1 data data_PET OPTIONS dist_rand

% load results
load PET_List1_Spearman_withExactSpatialP.mat

writematrix(dist_r{1,1}, 'PET_result/01_5HT1a_WAY_HC36.csv')
writematrix(dist_r{1,2}, 'PET_result/02_5HT1a_cumi_hc8_beliveau.csv')
writematrix(dist_r{1,3}, 'PET_result/03_5HT1b_P943_HC22.csv')
writematrix(dist_r{1,4}, 'PET_result/04_5HT1b_az_hc36_beliveau.csv')
writematrix(dist_r{1,5}, 'PET_result/05_5HT2a_ALT_HC19.csv')
writematrix(dist_r{1,6}, 'PET_result/06_5HT2a_cimbi_hc29_beliveau.csv')
writematrix(dist_r{1,7}, 'PET_result/07_5HT4_sb20_hc59_beliveau.csv')
writematrix(dist_r{1,8}, 'PET_result/08_CB1_FMPEPd2_hc22_laurikainen.csv')
writematrix(dist_r{1,9}, 'PET_result/09_CBF_ASL_MRI.csv')
writematrix(dist_r{1,10}, 'PET_result/10_D1_SCH23390_c11.csv')
writematrix(dist_r{1,11}, 'PET_result/11_D2_RACLOPRIDE_c11.csv')
writematrix(dist_r{1,12}, 'PET_result/12_D2_fallypride_hc49_jaworska.csv')
writematrix(dist_r{1,13}, 'PET_result/13_DAT_DATSPECT.csv')
writematrix(dist_r{1,14}, 'PET_result/14_FDOPA_f18.csv')
writematrix(dist_r{1,15}, 'PET_result/15_GABAa_FLUMAZENIL_c11.csv')
writematrix(dist_r{1,16}, 'PET_result/16_GABAa_flumazenil_hc16_norgaard.csv')
writematrix(dist_r{1,17}, 'PET_result/17_MU_CARFENTANIL_c11.csv')
writematrix(dist_r{1,18}, 'PET_result/18_MU_carfentanil_hc39_turtonen.csv')
writematrix(dist_r{1,19}, 'PET_result/19_NAT_MRB_c11.csv')
writematrix(dist_r{1,20}, 'PET_result/20_SERT_DASB_HC30.csv')
writematrix(dist_r{1,21}, 'PET_result/21_SERT_MADAM_c11.csv')
writematrix(dist_r{1,22}, 'PET_result/22_SERT_dasb_hc100_beliveau.csv')
writematrix(dist_r{1,23}, 'PET_result/23_VAChT_feobv_hc18_aghourian.csv')
writematrix(dist_r{1,24}, 'PET_result/24_VAChT_feobv_hc4_tuominen.csv')
writematrix(dist_r{1,25}, 'PET_result/25_VAChT_feobv_hc5_bedard.csv')
writematrix(dist_r{1,26}, 'PET_result/26_mGluR5_abp_hc22_rosaneto.csv')
writematrix(dist_r{1,27}, 'PET_result/27_mGluR5_abp_hc28_dubois.csv')
writematrix(dist_r{1,28}, 'PET_result/28_mGluR5_abp_hc73_smart.csv')