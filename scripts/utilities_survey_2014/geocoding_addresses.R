# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------

source('3 Scripts/general_functions.R')


loadpackage('rgdal')
loadpackage('sf')
loadpackage('dplyr')



## 1.2 load data --------

### data folder
rdir <- '2 Data/1 Raw'
pdir <- '2 Data/2 Processed'

### Address points from PICC data


load(file= file.path(pdir, 'Urban_PICC_PortailWal_Wal/Urban_PICC_PortailWal_Wal.Rdata'))


### addresses from Aquawal

surv14ads <- read.csv(file = file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Addresses/Surv14_ads.csv'))



# 2. geocoding --------------

## 2.1. manually edit -----------

### correct some addresses of some households

surv14ads[surv14ads$id %in% 132, c('street')] <- c('RUE JEAN MICHEL COURARD')

surv14ads[surv14ads$id %in% 155, c('street')] <- c('RUE OSCAR GILMANT')

surv14ads[surv14ads$id %in% 208, c('street')] <- c('RUE POYOUX SARTS')

surv14ads[surv14ads$id %in% 210, c('street')] <- c('RUE DES QUATRE CENTS BONNIERS')

surv14ads[surv14ads$id %in% 219, c('street')] <- c('KONIG ALBERT ALLEE')

surv14ads[surv14ads$id %in% 231, c('street')] <- c('RUE DES CHAMPS DU BOIS')

surv14ads[surv14ads$id %in% 239, c('street')] <- c('ROUTE DU MOULIN DE DISON')

surv14ads[surv14ads$id %in% 294, c('street')] <- c('BATTEGNEE')

surv14ads[surv14ads$id %in% 296, c('street')] <- c('RUE AUX RUELLES')

surv14ads[surv14ads$id %in% 317, c('street')] <- c('PLACE VALERE GRIMONPONT')

surv14ads[surv14ads$id %in% 323, c('street')] <- c('RUE DU 18 SEPTEMBRE 1794')

surv14ads[surv14ads$id %in% 393, c('street')] <- c('RUE POYOUX SARTS')

surv14ads[surv14ads$id %in% 403, c('street')] <- c("ROWE DE BATI RUE DU BATI")

surv14ads[surv14ads$id %in% 439, c('street')] <- c("RUE DERRIERE LE CHATEAU")

surv14ads[surv14ads$id %in% 476, c('street')] <- c("RUE MAXIMILIEN WATTELAR")

surv14ads[surv14ads$id %in% 478, c('street')] <- c("RUE EMILE VANDERVELDE")

surv14ads[surv14ads$id %in% 485, c('street')] <- c("RUE DE L'ARBRE SAINTE BARBE")

surv14ads[surv14ads$id %in% 490, c('street')] <- c("RUE JOSEPH DEDERICH")

surv14ads[surv14ads$id %in% 533, c('street')] <- c("RUE DU PUITS SAINTE ANNE")

surv14ads[surv14ads$id %in% 535, c('street')] <- c("ROUTE MILITAIRE")

surv14ads[surv14ads$id %in% 539, c('street')] <- c("AUX ANCIENNES PATURES")

surv14ads[surv14ads$id %in% 540, c('street')] <- c("RUE DES OISEAUX")

surv14ads[surv14ads$id %in% 557, c('street')] <- c("RUE JOSEPH JEAN MERLOT")

surv14ads[surv14ads$id %in% 562, c('street')] <- c("CHEMIN DE GRAND HALLEUX")

surv14ads[surv14ads$id %in% 587, c('street')] <- c("AVENUE DU ONZIEME ZOUAVES")

surv14ads[surv14ads$id %in% 696, c('street')] <- c("RUE D'HARMIGNIES")

surv14ads[surv14ads$id %in% 738, c('street')] <- c("RUE TERRIENNE")

surv14ads[surv14ads$id %in% 747, c('street')] <- c("RUE DU PARC SAINT ROCH")

surv14ads[surv14ads$id %in% 748, c('street')] <- c("RUE LOUIS DE BROUCKERE")

surv14ads[surv14ads$id %in% 758, c('street')] <- c("AVENUE D' ESNEUX")

surv14ads[surv14ads$id %in% 814, c('street')] <- c("RUE DE CHARLEROI")

surv14ads[surv14ads$id %in% 818, c('street')] <- c("RUE DE COQUIANE")

surv14ads[surv14ads$id %in% 857, c('street')] <- c("RUE DU FORT DE LONCIN")

surv14ads[surv14ads$id %in% 879, c('street')] <- c("RUE VICTOR DEWEZ")

surv14ads[surv14ads$id %in% 887, c('street')] <- c("RUE ARBRE SAINT PIERRE")

surv14ads[surv14ads$id %in% 904, c('street')] <- c("RUE DE L'EPINE")

surv14ads[surv14ads$id %in% 910, c('street')] <- c("RUE DU MONUMENT")

surv14ads[surv14ads$id %in% 917, c('street')] <- c("CHEMIN DE LA BLANCHE")

surv14ads[surv14ads$id %in% 919, c('street')] <- c("RESIDENCE MARCEL CARBONNELLE")

surv14ads[surv14ads$id %in% 950, c('street')] <- c("AVENUE JULES CARLIER")

surv14ads[surv14ads$id %in% 982, c('street')] <- c("RUE EUGENE DUMONT DE CHASSART")

surv14ads[surv14ads$id %in% 1033, c('street')] <- c("RUE LOUIS DE BROUCKERE")

surv14ads[surv14ads$id %in% 1035, c('street')] <- c("AVENUE DES CHEVALIERS D'ESCALADA")

surv14ads[surv14ads$id %in% 1087, c('street')] <- c("RUELLE DES FOSSES")

surv14ads[surv14ads$id %in% 1129, c('street')] <- c("BOURCY")

surv14ads[surv14ads$id %in% 1146, c('street')] <- c("AVENUE DE LA LIBERATION")

surv14ads[surv14ads$id %in% 1183, c('street')] <- c("ROUTE DE HOUSSE")

surv14ads[surv14ads$id %in% 1219, c('street')] <- c("PARC DOCTEUR JEAN BARZIN")

surv14ads[surv14ads$id %in% 1231, c('street')] <- c("RUE DE L'ANCIENNE ECOLE")

surv14ads[surv14ads$id %in% 1252, c('street')] <- c("LE PAS DE LOUP")

surv14ads[surv14ads$id %in% 1309, c('street')] <- c("RUE DE L'AURORE")

surv14ads[surv14ads$id %in% 1436, c('street')] <- c("RUE D'ARGENT")

surv14ads[surv14ads$id %in% 1454, c('street')] <- c("AVENUE BENONI VAN BEETHOVEN")

surv14ads[surv14ads$id %in% 1474, c('street')] <- c("RUE DU BOIS LINETTE")

surv14ads[surv14ads$id %in% 1486, c('street')] <- c("RUE ALEXANDRE DELSAMME")

surv14ads[surv14ads$id %in% 1540, c('street')] <- c("RUE JOSEPH JEAN MERLOT")

surv14ads[surv14ads$id %in% 1547, c('street')] <- c("AL'RODJE CREU")

surv14ads[surv14ads$id %in% 1548, c('street')] <- c("RUE AUGUSTIN FRANCOIS VILLERS")

surv14ads[surv14ads$id %in% 1594, c('street')] <- c("RUE DOCTEUR HUBERT DUBOIS")

surv14ads[surv14ads$id %in% 1597, c('street')] <- c("RUE DE LUMSONRY 2E AVENUE")

surv14ads[surv14ads$id %in% 1605, c('street')] <- c("IMPASSE DU CHATEAU D'EAU")

surv14ads[surv14ads$id %in% 1614, c('street')] <- c("RUE DU BOIS D'HAWIA")

surv14ads[surv14ads$id %in% 1636, c('street')] <- c("RUE LEON AUBRY")

surv14ads[surv14ads$id %in% 1647, c('street')] <- c("RUE VIRGILE BERENS")

surv14ads[surv14ads$id %in% 1674, c('street')] <- c("RUE X, DUMONT DE CHASSART")

surv14ads[surv14ads$id %in% 1700, c('street')] <- c("RUE DE LA BASSE MARIHAYE")

surv14ads[surv14ads$id %in% 1748, c('street')] <- c("RUE DU CHENE")

surv14ads[surv14ads$id %in% 1771, c('street')] <- c("ROUTE D'AUBEL")

surv14ads[surv14ads$id %in% 1825, c('street')] <- c("ALLEE DE LA CENSE ROUGE")

surv14ads[surv14ads$id %in% 1870, c('street')] <- c("ROUTE D'AUBEL")

surv14ads[surv14ads$id %in% 1887, c('street')] <- c("VIEILLE VOIE DE TONGRES")

surv14ads[surv14ads$id %in% 1976, c('street')] <- c("AVENUE DU PRESLE")

surv14ads[surv14ads$id %in% 2091, c('street')] <- c("RUE ALBERT ET LOUIS CURVERS")

surv14ads[surv14ads$id %in% 2129, c('street')] <- c("RUE SAINT JOSEPH")

surv14ads[surv14ads$id %in% 2198, c('street')] <- c("RUE ALPHONSE DELONNOY")

surv14ads[surv14ads$id %in% 2238, c('street')] <- c("RUE DE L'YSER")

surv14ads[surv14ads$id %in% 2268, c('street')] <- c("RUE DES GEANTS")

surv14ads[surv14ads$id %in% 2284, c('street')] <- c("RUE DE LA LONGUE COUTURE")

surv14ads[surv14ads$id %in% 2293, c('street')] <- c("CHEMIN D'YVES GOMEZEE")

surv14ads[surv14ads$id %in% 2361, c('street')] <- c("AVENUE ALBERT IER")

surv14ads[surv14ads$id %in% 2409, c('street')] <- c("RUE DE MONPLAISIR")

surv14ads[surv14ads$id %in% 2435, c('street')] <- c("RUE JULES HAMOIR")

surv14ads[surv14ads$id %in% 2465, c('street')] <- c("RUE DE LA CULEE")

surv14ads[surv14ads$id %in% 2470, c('street')] <- c("RUE COLLINET")

surv14ads[surv14ads$id %in% 2482, c('street')] <- c("RUE DES TROIS SERGENTS")

surv14ads[surv14ads$id %in% 2525, c('nb')] <- c("1")

surv14ads[surv14ads$id %in% 2572, c('street')] <- c("RUE HOULETTE")

surv14ads[surv14ads$id %in% 2578, c('street')] <- c("RUE DU BOURGEMESTRE HENRI FRANCOTTE")

surv14ads[surv14ads$id %in% 2593, c('street')] <- c("RUE DE L'AGASSE")

surv14ads[surv14ads$id %in% 2620, c('street')] <- c("RUE DES HAILLETTES")

surv14ads[surv14ads$id %in% 2645, c('street')] <- c("AVENUE DU ROLY DU SEIGNEUR")

surv14ads[surv14ads$id %in% 2659, c('street')] <- c("RUE DELCAUWE")

surv14ads[surv14ads$id %in% 2673, c('street')] <- c("ALLEE DE LA FERME DU BERCUIT")

surv14ads[surv14ads$id %in% 2689, c('street')] <- c("PLACE FRANCOIS QUINCHON")

surv14ads[surv14ads$id %in% 2710, c('street')] <- c("AVENUE DU CLOS SADIN")

surv14ads[surv14ads$id %in% 2711, c('street')] <- c("RUE LOUIS MARECHAL")

surv14ads[surv14ads$id %in% 2723, c('street')] <- c("AVENUE COMTE GERARD D'URSEL")

surv14ads[surv14ads$id %in% 2822, c('street')] <- c("RUE DE LA CHAUDRONNERIE")

## 2.2 geocoding by merging ------

surv14_coords <- dplyr::left_join(surv14ads, ads_point_wal)


# does not find coordinates 

nocoord <- surv14_coords[is.na(surv14_coords$X) & !is.na(surv14_coords$address),]



## 2.3.same address provide 2 sets of coordinate in PICC data ------------

dupid <- as.factor(surv14_coords[duplicated(surv14_coords$id),]$id)

dupiddf <- surv14_coords[surv14_coords$id %in% dupid,]

surv14_coords[surv14_coords$id %in% dupid, 15:21 ]<- NA

surv14_coords <- surv14_coords[!is.na(surv14_coords$X),]

## 2.4. convert to spatial object ------------

surv14_coords <- st_as_sf(surv14_coords, coords = c('X', 'Y'), crs = st_crs(ads_point_wal))

# 3. save results -----------

save(surv14_coords, file= file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Addresses/surv14_coord_PICC.Rdata'))



