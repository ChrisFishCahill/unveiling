/*
*'BERTA Cannibalism model
*Also includes newton-raphson algorithm for Req
*Bayesian Estimation of Recruitment Trends in Alberta
*Cahill & Walters October 2020
*k = lake, t = year, a = age, i = survey
*/
data {
  int <lower=0> n_surveys;             // number of surveys, i.e., rows of caa matrix
  int <lower=0> n_ages;                // number of ages 
  int <lower=0> n_obs;                 // n_ages*n_surveys 
  int <lower=0> n_years;               // number of years 
  int <lower=0> n_lakes;               // number of years 
  int <lower=0> caa[n_surveys, n_ages];// catch at age matrix 
  vector[n_surveys] prop_aged;         // proportion critters aged in survey k,t
  vector[n_surveys] effort;            // survey effort in k,t
  int lake[n_surveys];                 // lake indicator 
  int year[n_surveys];                 // year indicator 
  int <lower=0> ages[n_ages];          // helper vector
  matrix[n_lakes, 2] survey_yrs;       // years for which to calculate rbar
  int<lower=1> which_year;             // deliniate early vs. late
  real<lower=0> v_prior_early;         // prior, early F
  real<lower=0> v_prior_late;          // prior, late F
  vector<lower=0>[2] prior_sigma_v;    // prior variance for F's (early and late)
  real<lower=0> R0_mean;               // prior mean, R0
  real<lower=0> R0_sd;                 // prior sd, R0
  real<lower=0> ar_sd;                 // prior sd, ar
  real<lower=0> prior_mean_w;          // prior sd
  real<lower=0> prior_sigma_w;         // prior sd
  vector<lower=0>[n_lakes] vbk;        // lake specific vbk's 
  vector<lower=0>[n_lakes] linf;       // lake specific linf's 
  vector<lower=0>[n_lakes] a50;        // lake specific female a50
  vector<lower=0>[n_lakes] wl_beta;    // lake specific wl beta
  real<lower=0> lbar;                  // global average linf
  real<lower=0> M;                     // Instantaneous natural mortality
  real<lower=0> theta;                 // Lorenzen exponent
  real<lower=0> phi;                   // vulnerability parameter (nets)
  real<lower=0> psi;                   // vulnerability parameter (angling)
  vector[2] G_bound;                   // bounds for G
  real<lower=0> get_SSB_obs;           // logical get SSB observed
  real<lower=0> prior_sigma_G;         // sd of G
  int<lower=0> length_Fseq;            // length of Fseq for generated quantities calcs
  vector[length_Fseq] Fseq;            // sequence of values to iterate across for Fmsy
  real<lower=0> cr_prior;              // compensation ratio prior 
  real<lower=0> wcann;                 // wt at cannibalism
  real<lower=0> cpow;                  // cannibalism exponent
  real<lower=0> Mcann_mean; 
  real<lower=0> Mcann_sd; 
}
transformed data {
  int<lower=0> caa_obs[n_obs];              // rowsums for survey k,t
  matrix<lower=0>[n_lakes, n_ages] v_a;     // net vulnerability age a
  matrix<lower=0>[n_lakes, n_ages] v_f_a;   // angling vulnerability age a
  matrix<lower=0>[n_lakes, n_ages] l_a;     // length at age a 
  matrix<lower=0>[n_lakes, n_ages] M_a;     // M at age a 
  matrix<lower=0>[n_lakes, n_ages] f_a;     // fec at age a
  matrix<lower=0>[n_lakes, n_ages] W_a;     // Weight at age a
  matrix<lower=0>[n_lakes, n_ages] can_a;   // Cannibalism at age a
  vector<lower=0>[n_lakes] sbr0;            // spawning biomass per recruit unfished 
  vector<lower=0>[n_lakes] cpro;            // spawning biomass per recruit unfished 
  vector[n_surveys] SSB_Cn;                 // SSB obs numerator
  vector[n_surveys] SSB_Cd;                 // SSB obs denominator
  vector[n_surveys] SSB_C;                  // SSB obs
  matrix[n_lakes, n_ages] Su_F0;            // survivorship unfished (F=0) 
  vector[n_lakes] ar_mean;                  // ar mean
  int counter = 0; 
  
  //calculate vul, length-age, M-age, fec-age matrices
  for(k in 1:n_lakes){
    sbr0[k] = 0; //initialize
    cpro[k] = 0;
    for(a in 1:n_ages){
      v_a[k, a] = ((linf[k]/lbar)*(1 - exp(-vbk[k] * ages[a])))^phi;          
      v_f_a[k, a] = ((linf[k]/lbar)*(1 - exp(-vbk[k] * ages[a])))^psi;  
      l_a[k, a] = (linf[k]/lbar)*(1 - exp(-vbk[k] * ages[a])); 
      M_a[k, a] = M/l_a[k, a]^theta;
      W_a[k, a] = 0.00001*(linf[k]*(1 - exp(-vbk[k] * ages[a])))^wl_beta[k];
      
      if(l_a[k,a]^wl_beta[k] >= wcann){
        can_a[k,a] = (l_a[k,a]^wl_beta[k] - wcann)^cpow; 
      } else {
        can_a[k,a] = 0; 
      }
      if(a < a50[k]){
        f_a[k, a] = 0; 
      } else { 
        //relative weight at age assumed to follow vb
        f_a[k, a] = fmax(0, (l_a[k, a]^wl_beta[k])); 
      }
      
      if(a == 1){ 
        Su_F0[k, a] = 1;
      } else{
        Su_F0[k, a] = Su_F0[k, a-1]*exp(-M_a[k,a-1]); 
      }
      sbr0[k] += f_a[k,a]*Su_F0[k,a]; 
      cpro[k] += can_a[k,a]*Su_F0[k,a]; 
    }
    ar_mean[k] = log(cr_prior/sbr0[k]); 
  }
  
  //calculate the rowsums for each k,t survey, observed SSB
  //SSB_C(k,t)=sum over a of fec(k,a)*C(k,a,t)/[vage(k,a)Nnet(k,t)Paged(k,t)]
  for(i in 1:n_surveys){
    int k_idx = lake[i];
    SSB_Cn[i] = 0; 
    SSB_Cd[i] = 0; 
    SSB_C[i] = 0; 
    for(a in 1:n_ages){
      counter = counter + 1; 
      SSB_Cn[i] += f_a[k_idx, a]*caa[i, a]*(1/v_a[k_idx, a]); 
      caa_obs[counter] = caa[i,a]; 
    }
    SSB_Cd[i] = prop_aged[i]*effort[i]; 
    SSB_C[i] = SSB_Cn[i] / SSB_Cd[i];
  }
}
parameters {
  matrix<lower=0>[n_lakes, 2] v;                         // Lake specific early [,1]/late F [,2]
  vector<lower=0>[n_lakes] R0;                           // recruitment sans F 
  matrix[n_lakes, n_years-2] w;                          // first 2 yrs for init
  vector<lower=G_bound[1], upper=G_bound[2]>[n_lakes] G; // is population at equilibrium (1) or declining (<1)
  vector<lower=0>[n_lakes] Mcann;                       
  vector[n_lakes] ar;                                    // stock-recruit a
  //real<lower=0> phi;                                   // for negative binomial
  //vector<lower=0, upper=1>[n_lakes] su_stock;          // attempting to estimate stocked fish survival
}
transformed parameters {
  matrix<lower=0>[n_lakes, n_years] F_mat;           // Insantaneous fishing mortality
  real Nat_array[n_ages, n_years, n_lakes];          // Numbers at age  
  matrix [n_lakes, n_years] SSB;                     // Spawning stock biomass
  matrix [n_lakes, n_years] canB;                     // Spawning stock biomass
  vector<lower=0>[n_lakes] br;                       // derived stock-recruit b from sbr0, R0
  vector<lower=0>[n_obs] caa_pred;                   // predicted catch at age
  matrix<lower=0>[n_lakes, n_years] R2;              // kick out recruits rather than N(a,t,k)
  matrix[n_lakes, n_ages] Su_Fearly;                 // survivorship including fishing   
  matrix[n_lakes, n_ages] Su_Flate;                  // survivorship including fishing       
  vector[n_lakes] sbrf_early;                        // spawning biomass per recruit fished
  vector[n_lakes] sbrf_late;                         // spawning biomass per recruit fished
  vector<lower=0>[n_lakes] pinit;                    // how much depletion from R0
  vector<lower=0>[n_lakes] Rinit;                    // initial recruitment
  vector[n_surveys] SSB_obs;                         // SSB observed
  matrix[n_lakes, n_years] pred_N_catch;             // predicted catch N/ha, *not vulN*
  matrix[n_lakes, n_years] pred_B_catch;             // predicted catch biomass/ha
  vector<lower=0>[n_lakes] sbr0_kick;                // sbr0 report 
  vector<lower=0>[n_lakes] cpro_kick;                // sbr0 report 
  vector[n_lakes] ar_mean_kick;                      // report the ar mean 
  vector<lower=0>[n_lakes] SPR;                      // spawning potential ratio
  vector<lower=0>[n_lakes] SSB_bar;                  // average ssb survey years
  vector<lower=0>[n_lakes] SBR;                      // spawning biomass ratio
  vector<lower=0>[n_lakes] counter_SSB;              // hack value for SBR calcs
  vector<lower=0>[n_lakes] cr;                       // compensation ratio kick out to check math
  vector<lower=0>[n_lakes] canr;
  vector<lower=0>[n_lakes] C0;
  vector[n_lakes] ar_pred;

  
  // calculate sbrf
  for(k in 1:n_lakes){
    sbr0_kick[k] = sbr0[k]; 
    cpro_kick[k] = cpro[k]; 
    ar_mean_kick[k] = ar_mean[k]; 
    sbrf_early[k] = 0; 
    sbrf_late[k] = 0; 
    for(a in 1:n_ages){
      if(a == 1){
        Su_Fearly[k, a] = 1; 
        Su_Flate[k, a] = 1; 
      } else {
        Su_Fearly[k, a] = Su_Fearly[k, a-1]*exp(-M_a[k,a-1] - v_f_a[k,a-1]*v[k,1]); 
        Su_Flate[k, a] = Su_Flate[k, a-1]*exp(-M_a[k,a-1] - v_f_a[k,a-1]*v[k,2]); 
      }
      sbrf_early[k] += f_a[k,a]*Su_Fearly[k, a]; 
      sbrf_late[k] += f_a[k,a]*Su_Flate[k, a]; 
    }
    SPR[k] = sbrf_late[k] / sbr0[k]; 
  }

  // Calculate recruitment b's, Rinit's 
  for(k in 1:n_lakes){
    cr[k] = exp(ar[k])*sbr0[k]; 
    C0[k] = R0[k]*cpro[k]; 
    canr[k] = Mcann[k] / C0[k]; 
    ar_pred[k] = cr[k]/sbr0[k]*exp(Mcann[k]);   
    br[k] = (exp(ar[k])*sbr0[k]-1) / (R0[k]*sbr0[k]);
    Rinit[k] = G[k]*R0[k]; 
    pinit[k] = Rinit[k] / R0[k]; 
  }

  //Initialize F(t) fishing rate vector from start year to 2018
  for(k in 1:n_lakes){
    for(t in 1:n_years){
      if(t < which_year){
        F_mat[k, t] = v[k, 1];
      }
      if(t >= which_year){
        F_mat[k, t] = v[k, 2];
      }
      SSB[k,t] = 0;  
      canB[k,t] = 0; 
      pred_N_catch[k, t] = 0; 
      pred_B_catch[k, t] = 0;
    }
  }
  
  //Initialize the Nat_array age structure for t = 1,2
  for(k in 1:n_lakes){
    for(t in 1:2){
      Nat_array[1, t, k] =  Rinit[k];
      if(t == 1){
        for(a in 2:n_ages){
          Nat_array[a, t, k] = Nat_array[a-1, t, k]*exp(-M_a[k, a-1] - v_f_a[k, a-1]*F_mat[k, 1]); 
          SSB[k, t] += Nat_array[a ,t, k]*f_a[k, a];
          canB[k,t] += Nat_array[a ,t, k]*can_a[k,a]; 
          pred_N_catch[k, t] += Nat_array[a ,t, k]*v_f_a[k, a];  
          pred_B_catch[k, t] += Nat_array[a ,t, k]*v_f_a[k, a]*W_a[k, a];  
        }
        pred_N_catch[k, t] = pred_N_catch[k, t]*(1-exp(-F_mat[k,1]));
        pred_B_catch[k, t] = pred_B_catch[k, t]*(1-exp(-F_mat[k,1]));
      }
      if(t == 2){
        for(a in 2:n_ages){
          Nat_array[a, t, k] = Nat_array[a, 1, k]; 
        }
      }
      SSB[k, t] = SSB[k, 1]; 
      pred_N_catch[k, t] = pred_N_catch[k, 1]; 
      pred_B_catch[k, t] = pred_B_catch[k, 1]; 
    }
  }
  
  //Calculate the N(a,t,lake) array and derived outputs
  for(k in 1:n_lakes){
    R2[k,1] = Nat_array[1, 1, k];
    R2[k,2] = Nat_array[1, 2, k];
    
    SSB_bar[k] = 0;
    counter_SSB[k] = 0; 

    for(t in 3:n_years){
      Nat_array[1, t, k] = ar_pred[k]*SSB[k, t-2]*exp(- canr[k]*canB[k,t-2] + w[k,t-2]) / (1 + br[k]*SSB[k, t-2]);  
      for(a in 2:n_ages){
        Nat_array[a, t, k] = Nat_array[a-1, t-1, k]*exp(-M_a[k, a-1] - v_f_a[k, a-1]*F_mat[k, t-1]);  
        SSB[k, t] += Nat_array[a ,t, k]*f_a[k, a];
        canB[k,t] += Nat_array[a ,t, k]*can_a[k,a]; 
        pred_N_catch[k, t] += Nat_array[a ,t, k]*v_f_a[k, a]; 
        pred_B_catch[k, t] += Nat_array[a ,t, k]*v_f_a[k, a]*W_a[k, a]; 
      }
      pred_N_catch[k, t] = pred_N_catch[k, t]*(1-exp(-F_mat[k,t]));
      pred_B_catch[k, t] = pred_B_catch[k, t]*(1-exp(-F_mat[k,t]));
      R2[k,t] = Nat_array[1, t, k];
      //calculate mean SSB across survey years
      if(t >= survey_yrs[k, 1] && t <= survey_yrs[k, 2]){
        counter_SSB[k] += 1; 
        SSB_bar[k] += SSB[k, t];
      }
    }
    SSB_bar[k] = SSB_bar[k] / counter_SSB[k];
    SBR[k] = SSB_bar[k] / (R0[k]*sbr0[k]);     
  }
  
  //Calculate the preds vector
  //C(k,a,t)=N(a,t,k)*Nnet(k,t)Paged(k,t)*v_a(k,a) 
  for(hack in 1:1){  //naughty hack to initialize j = 0
    int j = 0; 
    for(i in 1:n_surveys){
      int k_idx = lake[i]; 
      for(a in 1:n_ages){
        j += 1;
        caa_pred[j] = 0; 
        caa_pred[j] = Nat_array[a, year[i], k_idx]*  //numbers(a,t,k)
        prop_aged[i]*                                //prop_aged
        effort[i]*                                   //survey effort
        v_a[k_idx, a];                               //vulnerability to gear
      }
      if(get_SSB_obs==1){
        SSB_obs[i] = SSB_C[i]; 
      }
    }
  }
}
model {
  //priors:
  v[,1] ~ normal(v_prior_early, prior_sigma_v[1]); 
  v[,2] ~ normal(v_prior_late, prior_sigma_v[2]); 
  R0 ~ lognormal(R0_mean, R0_sd); 
  for(k in 1:n_lakes){
    ar[k] ~ normal(ar_mean[k], ar_sd);
  }
  G ~ normal(0,prior_sigma_G); 
  to_vector(w) ~ normal(prior_mean_w, prior_sigma_w); 
  Mcann ~ normal(Mcann_mean,Mcann_sd); 
  
  //likelihood
  caa_obs ~ poisson(caa_pred); 
  
  //phi ~ cauchy(0,3);
  //su_stock ~ beta(2,2); 
  //caa_obs ~ neg_binomial_2(caa_pred, phi); 
}
generated quantities{
  vector[n_lakes] Fmsy;           // instantaneous fishing mortality @ MSY                
  vector[n_lakes] MSY;            // weight yield kg/ha
  vector[n_lakes] F_ratio;        // Flate / F_msy  
  vector[n_lakes] F_early_ratio;  // Fearly / F_msy                        
  vector[n_lakes] b_ratio;        // average ssb survey years / pristine ssb

  //Fmsy[k], MSY[k] subroutine
  for(k in 1:n_lakes){
    Fmsy[k] = 0; 
    MSY[k] = 0; 
    for(i in 1:length_Fseq){
      real sbrf = 0; 
      real ypr = 0; 
      real cpr = 0; 
      real su = 1; 
      real Req = 3; //cannot set Req too low or else newton finds trivial solution 
      real g_x = 0.1; 
      real g_prime = 0; 
      real Yeq = 0; 
      real arp = 0; 
      arp = log(cr[k]/sbr0[k]) + Mcann[k]; 
      
      for(a in 1:n_ages){
        sbrf += su*f_a[k,a]; //accumulate spawning biomass per recruit
        ypr += su*(1-exp(-Fseq[i]*v_f_a[k,a]))*W_a[k,a]; 
        cpr += su*can_a[k,a]; 
        su = su*exp(-M_a[k,a] - Fseq[i]*v_f_a[k,a]); 
      }
      
      //hand code jank newton-raphson algorithm
      while(fabs(g_x) > 0.01){
        g_x = Req - Req*sbrf*exp(arp-canr[k]*Req*cpr)/(1+ br[k]*Req*sbrf); 
        g_prime = 1 - ((sbrf*exp(arp-canr[k]*Req*cpr)) / (1 + br[k]*Req*sbrf)) * (1-canr[k]*Req*cpr - ( br[k]*Req*sbrf)/(1+ br[k]*Req*sbrf)); 
        Req = Req - g_x/g_prime; 
      }

      Yeq = Req*ypr; //predicted equilibrium yield
      if(Yeq > MSY[k]){
        MSY[k] = Yeq; 
        //jitter values to make a purdy histogram:
        Fmsy[k]=Fseq[i] + 0.01*(uniform_rng(0,1)-0.5); 
      } else {
        //Yeq for this F is lower than highest value already found,
        //so can exit the subroutine
        continue; 
      }
    }

    //Kobe plot hogwash
    F_early_ratio[k] = v[k,1] / Fmsy[k];
    F_ratio[k] = v[k,2] / Fmsy[k];
    b_ratio[k] = SSB_bar[k] / (R0[k]*sbr0[k]);
  }
}
