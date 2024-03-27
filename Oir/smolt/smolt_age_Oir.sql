select 
    extract(year from cam_date_heure_fin) as annee,
    cap_lf,
    age_riv,
    count(*) as nb
from oir.v_captures
where 
    sec_code='CERISEL_PGD' and tax_code='SAT' and phenotype in ('JDIS', 'JDIP') AND
    cap_is_recapture='false'
group by annee, age_riv, cap_lf
;

--> fichier smolt_age_brut.csv