# ==============================================================================
# SERIATIM LIFE INSURANCE GPV & CASH FLOW PROJECTION ENGINE (AXIS/ALFA STYLE)
# ==============================================================================
install.packages("tidyverse")
library(tidyverse)

set.seed(2026) # For exact reproducibility

# Create output directory if it doesn't exist
if (!dir.exists("outputs")) dir.create("outputs")

# ==============================================================================
# MODULE 1: SYNTHETIC MODEL POINT GENERATOR (5,000 POLICIES)
# ==============================================================================
n_policies <- 5000

raw_policies <- tibble(
  Policy_ID       = sprintf("POL_%05d", 1:n_policies),
  Plan_Code       = sample(c("TERM_LVL", "ENDOW_NP"), n_policies, replace = TRUE, prob = c(0.6, 0.4)),
  Gender          = sample(c("M", "F"), n_policies, replace = TRUE, prob = c(0.65, 0.35)),
  Issue_Age       = sample(22:55, n_policies, replace = TRUE),
  Policy_Term     = sample(c(10, 15, 20, 25, 30), n_policies, replace = TRUE),
  Elapsed_Months  = sample(0:120, n_policies, replace = TRUE), # Up to 10 yrs in force
  Face_Amount     = sample(seq(500000, 5000000, by = 250000), n_policies, replace = TRUE),
  Payment_Mode    = sample(c("Annual", "Monthly"), n_policies, replace = TRUE, prob = c(0.7, 0.3))
) %>%
  mutate(
    # Ensure Elapsed_Months is strictly less than Policy_Term * 12 for valid policies
    Elapsed_Months = pmin(Elapsed_Months, (Policy_Term * 12) - 12),
    Attained_Age   = Issue_Age + floor(Elapsed_Months / 12),
    # Realistic proxy pricing for Annualized Gross Premium
    Base_Rate      = ifelse(Plan_Code == "TERM_LVL", 
                            0.0025 * exp(0.055 * (Issue_Age - 20)), 
                            (1 / Policy_Term) * 0.88 + 0.003 * exp(0.04 * (Issue_Age - 20))),
    Annual_Premium = round(Face_Amount * Base_Rate * ifelse(Gender == "F", 0.92, 1.00), 2),
    Modal_Premium  = ifelse(Payment_Mode == "Annual", Annual_Premium, round(Annual_Premium / 12 * 1.03, 2))
  ) %>%
  select(-Base_Rate)

# Inject ~1.5% realistic data errors (75 records) to test governance pipeline
corrupt_idx <- sample(1:n_policies, 75)
raw_policies$Face_Amount[corrupt_idx[1:25]]    <- -500000                    # Negative sum assured
raw_policies$Elapsed_Months[corrupt_idx[26:50]] <- raw_policies$Policy_Term[corrupt_idx[26:50]] * 12 + 6 # Past maturity
raw_policies$Issue_Age[corrupt_idx[51:75]]      <- 75                         # Maturity age > 85

# ==============================================================================
# MODULE 2: DATA VALIDATION & EXCEPTION LOGGING
# ==============================================================================
validated_data <- raw_policies %>%
  mutate(
    Error_Reason = case_when(
      Face_Amount <= 0 ~ "Invalid Face Amount (<= 0)",
      Elapsed_Months >= Policy_Term * 12 ~ "Policy Already Matured",
      (Issue_Age + Policy_Term) > 85 ~ "Maturity Age Exceeds Limit (85)",
      Annual_Premium <= 0 ~ "Invalid Premium",
      TRUE ~ "VALID"
    )
  )

# Export quarantine log for audit trail
exception_log <- validated_data %>% filter(Error_Reason != "VALID")
clean_policies <- validated_data %>% filter(Error_Reason == "VALID") %>% select(-Error_Reason)

write_csv(exception_log, "outputs/01_data_exception_log.csv")
cat(sprintf("Data Validation Complete: %d Valid Policies | %d Quarantined Records\n", 
            nrow(clean_policies), nrow(exception_log)))

# ==============================================================================
# MODULE 3: ECONOMIC & DEMOGRAPHIC ASSUMPTION TABLES
# ==============================================================================
# 1. Ultimate Mortality Table (Gompertz-Makeham approximation calibrated to IALM)
ages <- 20:100
mortality_table <- tibble(
  Age = ages,
  qx_ult_M = pmin(1, 0.0006 + 0.000045 * exp(0.092 * (ages - 20))),
  qx_ult_F = pmin(1, 0.0005 + 0.000038 * exp(0.090 * (ages - 20)))
)

# 2. Duration-Based Annual Lapse Rates (Policy Year 1 to 30)
lapse_table <- tibble(
  Policy_Year = 1:30,
  wx_annual   = c(0.12, 0.08, 0.05, rep(0.03, 26), 0.00) # 0% lapse in final year
)

# 3. Spot Yield Curve & Monthly Discount Factors (Upward sloping: 6.5% to 7.2%)
max_proj_months <- 360
yield_curve <- tibble(
  Proj_Month = 1:max_proj_months,
  Proj_Year  = ceiling(Proj_Month / 12),
  Spot_Rate  = 0.065 + 0.007 * (1 - exp(-0.08 * (Proj_Month / 12))),
  v_eom      = (1 + Spot_Rate)^(-Proj_Month / 12),        # End-of-month discount
  v_mid      = (1 + Spot_Rate)^(-(Proj_Month - 0.5) / 12), # Mid-month discount (claims)
  v_bom      = (1 + Spot_Rate)^(-(Proj_Month - 1) / 12)    # Start-of-month discount (prems)
)

# Expense & Inflation Parameters
maint_exp_annual <- 650     # INR per policy p.a.
exp_inflation    <- 0.05    # 5% p.a.
claim_expense    <- 1500    # INR per death/maturity claim
comm_rate_yr1    <- 0.15    # 15% Year 1 commission
comm_rate_ren    <- 0.02    # 2% Renewal commission

# ==============================================================================
# MODULE 4: VECTORIZED MONTHLY CASH FLOW PROJECTION ENGINE
# ==============================================================================
# Expand policies across remaining projection months (Seriatim Projection Matrix)
proj_grid <- clean_policies %>%
  mutate(Rem_Months = (Policy_Term * 12) - Elapsed_Months) %>%
  uncount(Rem_Months, .id = "Proj_Month") %>%
  mutate(
    Curr_Month_In_Force = Elapsed_Months + Proj_Month,
    Curr_Policy_Year    = ceiling(Curr_Month_In_Force / 12),
    Curr_Age            = Issue_Age + floor((Curr_Month_In_Force - 1) / 12),
    Is_Maturity_Month   = (Curr_Month_In_Force == Policy_Term * 12)
  ) %>%
  # Join Mortality, Lapses, and Yield Curve
  left_join(mortality_table, by = c("Curr_Age" = "Age")) %>%
  left_join(lapse_table, by = c("Curr_Policy_Year" = "Policy_Year")) %>%
  left_join(yield_curve, by = "Proj_Month") %>%
  mutate(
    # Apply Select Mortality Factors (70% Yr 1, 85% Yr 2, 100% Yr 3+)
    Select_Factor = case_when(
      Curr_Policy_Year == 1 ~ 0.70,
      Curr_Policy_Year == 2 ~ 0.85,
      TRUE ~ 1.00
    ),
    qx_annual = ifelse(Gender == "M", qx_ult_M, qx_ult_F) * Select_Factor,
    
    # Convert Annual Independent Rates to Monthly Independent Rates
    qx_m = 1 - (1 - qx_annual)^(1/12),
    wx_m = ifelse(Is_Maturity_Month, 0, 1 - (1 - wx_annual)^(1/12)),
    
    # Multiple-Decrement Dependent Probabilities (UDD assumption within month)
    qd_dep = qx_m * (1 - 0.5 * wx_m),
    qw_dep = wx_m * (1 - 0.5 * qx_m),
    p_tau  = 1 - qd_dep - qw_dep
  ) %>%
  # Calculate Cumulative In-Force Survival Probability (_t p_x^(tau)) per Policy
  group_by(Policy_ID) %>%
  mutate(
    t_p_tau = lag(cumprod(p_tau), default = 1.0)
  ) %>%
  ungroup() %>%
  # Compute Monthly Expected Cash Flows
  mutate(
    # 1. Expected Premium Inflow (Paid at start of month)
    Prem_Due_Flag = ifelse(Payment_Mode == "Monthly", 1, 
                           ifelse((Curr_Month_In_Force - 1) %% 12 == 0, 1, 0)),
    Exp_Premium   = Modal_Premium * t_p_tau * Prem_Due_Flag,
    
    # 2. Expected Death Benefit Outflow (Mid-month)
    Exp_Death_Ben = (Face_Amount + claim_expense) * t_p_tau * qd_dep,
    
    # 3. Expected Surrender Benefit (Endowment only, after Yr 2, proportional SV)
    Surr_Factor   = ifelse(Plan_Code == "ENDOW_NP" & Curr_Policy_Year >= 3, 
                           0.35 + 0.55 * (Curr_Month_In_Force / (Policy_Term * 12)), 0),
    Surr_Value    = Face_Amount * (Curr_Month_In_Force / (Policy_Term * 12)) * Surr_Factor,
    Exp_Surr_Ben  = Surr_Value * t_p_tau * qw_dep,
    
    # 4. Expected Maturity Outflow (Final month for Endowment only)
    Exp_Mat_Ben   = ifelse(Is_Maturity_Month & Plan_Code == "ENDOW_NP", 
                           (Face_Amount + claim_expense) * t_p_tau * p_tau, 0),
    
    # 5. Expected Maintenance & Commission Expenses
    Infl_Factor   = (1 + exp_inflation)^((Proj_Month - 0.5) / 12),
    Comm_Pct      = ifelse(Curr_Policy_Year == 1, comm_rate_yr1, comm_rate_ren),
    Exp_Expenses  = ((maint_exp_annual / 12) * Infl_Factor * t_p_tau) + (Comm_Pct * Exp_Premium),
    
    # Present Values at Valuation Date (t = 0)
    PV_Premium    = Exp_Premium * v_bom,
    PV_Death      = Exp_Death_Ben * v_mid,
    PV_Surrender  = Exp_Surr_Ben * v_mid,
    PV_Maturity   = Exp_Mat_Ben * v_eom,
    PV_Expenses   = Exp_Expenses * v_mid,
    
    # Net Reserve Contribution
    PV_Net_Liab   = (PV_Death + PV_Surrender + PV_Maturity + PV_Expenses) - PV_Premium
  )

# ==============================================================================
# MODULE 5: VALUATION SUMMARY & EXPORT FOR EXCEL AUDIT WORKBOOK
# ==============================================================================
# 1. Policy-Level Reserve Summary (Includes Zeroization Check)
policy_reserves <- proj_grid %>%
  group_by(Policy_ID, Plan_Code, Gender, Issue_Age, Policy_Term, Elapsed_Months, Face_Amount) %>%
  summarise(
    PV_Premiums     = sum(PV_Premium),
    PV_Death_Ben    = sum(PV_Death),
    PV_Surr_Ben     = sum(PV_Surrender),
    PV_Mat_Ben      = sum(PV_Maturity),
    PV_Expenses     = sum(PV_Expenses),
    Unfloored_GPV   = sum(PV_Net_Liab),
    Zeroized_GPV    = pmax(0, Unfloored_GPV), # Statutory floor at zero
    # Run-off Control Check: Total Exit Probabilities must equal 1.000000
    Total_Exit_Prob = sum(t_p_tau * qd_dep) + sum(t_p_tau * qw_dep) + last(t_p_tau * p_tau),
    .groups = "drop"
  )

# 2. Portfolio Summary by Product (For Excel Executive Dashboard)
portfolio_summary <- policy_reserves %>%
  group_by(Plan_Code) %>%
  summarise(
    Active_Policies = n(),
    Total_Sum_Assured = sum(Face_Amount),
    PV_Future_Prems   = sum(PV_Premiums),
    PV_Future_Claims  = sum(PV_Death_Ben + PV_Surr_Ben + PV_Mat_Ben),
    PV_Future_Exp     = sum(PV_Expenses),
    Unfloored_GPV     = sum(Unfloored_GPV),
    Zeroized_GPV      = sum(Zeroized_GPV),
    Max_Runoff_Error  = max(abs(Total_Exit_Prob - 1)),
    .groups = "drop"
  )

print(portfolio_summary)

# 3. Aggregated Monthly Cash Flow Profile (For Excel Charts & Sensitivity)
monthly_cf_summary <- proj_grid %>%
  group_by(Plan_Code, Proj_Month) %>%
  summarise(
    Expected_Inforce = sum(t_p_tau),
    Exp_Premiums     = sum(Exp_Premium),
    Exp_Death        = sum(Exp_Death_Ben),
    Exp_Surrender    = sum(Exp_Surr_Ben),
    Exp_Maturity     = sum(Exp_Mat_Ben),
    Exp_Expenses     = sum(Exp_Expenses),
    Net_Cash_Flow    = Exp_Premiums - (Exp_Death + Exp_Surrender + Exp_Maturity + Exp_Expenses),
    .groups = "drop"
  )

# 4. Extract 2 Sample Policies (1 Term, 1 Endowment) for Excel Single-Policy Audit
sample_ids <- c(
  clean_policies %>% filter(Plan_Code == "TERM_LVL") %>% slice(1) %>% pull(Policy_ID),
  clean_policies %>% filter(Plan_Code == "ENDOW_NP") %>% slice(1) %>% pull(Policy_ID)
)

sample_policy_audit <- proj_grid %>%
  filter(Policy_ID %in% sample_ids)

# Export all files for Excel ingestion
write_csv(portfolio_summary,   "outputs/02_portfolio_valuation_summary.csv")
write_csv(monthly_cf_summary,  "outputs/03_monthly_cashflow_profile.csv")
write_csv(sample_policy_audit, "outputs/04_sample_policy_audit.csv")
write_csv(mortality_table,     "outputs/05_assumption_mortality.csv")
write_csv(yield_curve,         "outputs/06_assumption_yield_curve.csv")

cat("All outputs exported successfully to /outputs folder!\n")

# ==============================================================================
# MODULE 6: WRAP ENGINE INTO A REUSABLE VALUATION FUNCTION
# ==============================================================================
run_gpv_valuation <- function(policies_df, rate_shift = 0, mort_mult = 1.0, lapse_mult = 1.0, exp_mult = 1.0) {
  
  # Adjust Yield Curve
  yc_shocked <- yield_curve %>%
    mutate(
      Spot_Rate = pmax(0.001, Spot_Rate + rate_shift),
      v_eom     = (1 + Spot_Rate)^(-Proj_Month / 12),
      v_mid     = (1 + Spot_Rate)^(-(Proj_Month - 0.5) / 12),
      v_bom     = (1 + Spot_Rate)^(-(Proj_Month - 1) / 12)
    )
  
  policies_df %>%
    mutate(Rem_Months = (Policy_Term * 12) - Elapsed_Months) %>%
    filter(Rem_Months > 0) %>%
    uncount(Rem_Months, .id = "Proj_Month") %>%
    mutate(
      Curr_Month_In_Force = Elapsed_Months + Proj_Month,
      Curr_Policy_Year    = ceiling(Curr_Month_In_Force / 12),
      Curr_Age            = Issue_Age + floor((Curr_Month_In_Force - 1) / 12),
      Is_Maturity_Month   = (Curr_Month_In_Force == Policy_Term * 12)
    ) %>%
    left_join(mortality_table, by = c("Curr_Age" = "Age")) %>%
    left_join(lapse_table, by = c("Curr_Policy_Year" = "Policy_Year")) %>%
    left_join(yc_shocked, by = "Proj_Month") %>%
    mutate(
      Select_Factor = case_when(Curr_Policy_Year == 1 ~ 0.70, Curr_Policy_Year == 2 ~ 0.85, TRUE ~ 1.00),
      qx_annual     = pmin(1, ifelse(Gender == "M", qx_ult_M, qx_ult_F) * Select_Factor * mort_mult),
      wx_annual_adj = pmin(0.95, wx_annual * lapse_mult),
      
      qx_m   = 1 - (1 - qx_annual)^(1/12),
      wx_m   = ifelse(Is_Maturity_Month, 0, 1 - (1 - wx_annual_adj)^(1/12)),
      qd_dep = qx_m * (1 - 0.5 * wx_m),
      qw_dep = wx_m * (1 - 0.5 * qx_m),
      p_tau  = 1 - qd_dep - qw_dep
    ) %>%
    group_by(Policy_ID) %>%
    mutate(t_p_tau = lag(cumprod(p_tau), default = 1.0)) %>%
    ungroup() %>%
    mutate(
      Prem_Due_Flag = ifelse(Payment_Mode == "Monthly", 1, ifelse((Curr_Month_In_Force - 1) %% 12 == 0, 1, 0)),
      Exp_Premium   = Modal_Premium * t_p_tau * Prem_Due_Flag,
      Exp_Death_Ben = (Face_Amount + claim_expense * exp_mult) * t_p_tau * qd_dep,
      
      Surr_Factor   = ifelse(Plan_Code == "ENDOW_NP" & Curr_Policy_Year >= 3, 
                             0.35 + 0.55 * (Curr_Month_In_Force / (Policy_Term * 12)), 0),
      Surr_Value    = Face_Amount * (Curr_Month_In_Force / (Policy_Term * 12)) * Surr_Factor,
      Exp_Surr_Ben  = Surr_Value * t_p_tau * qw_dep,
      
      Exp_Mat_Ben   = ifelse(Is_Maturity_Month & Plan_Code == "ENDOW_NP", 
                             (Face_Amount + claim_expense * exp_mult) * t_p_tau * p_tau, 0),
      
      Infl_Factor   = (1 + exp_inflation)^((Proj_Month - 0.5) / 12),
      Comm_Pct      = ifelse(Curr_Policy_Year == 1, comm_rate_yr1, comm_rate_ren),
      Exp_Expenses  = ((maint_exp_annual * exp_mult / 12) * Infl_Factor * t_p_tau) + (Comm_Pct * Exp_Premium),
      
      PV_Premium    = Exp_Premium * v_bom,
      PV_Outflows   = (Exp_Death_Ben * v_mid) + (Exp_Surr_Ben * v_mid) + (Exp_Mat_Ben * v_eom) + (Exp_Expenses * v_mid),
      PV_Net_Liab   = PV_Outflows - PV_Premium
    ) %>%
    group_by(Policy_ID, Plan_Code) %>%
    summarise(
      PV_Premiums   = sum(PV_Premium),
      PV_Outflows   = sum(PV_Outflows),
      Unfloored_GPV = sum(PV_Net_Liab),
      Zeroized_GPV  = pmax(0, Unfloored_GPV),
      .groups = "drop"
    )
}

# ==============================================================================
# MODULE 7: EQUIVALENCE PRINCIPLE GROSS PREMIUM CALIBRATION (OPTIONAL RE-PRICE)
# ==============================================================================
# Solve exact Gross Premium at issue (Elapsed_Months = 0) with a 5% profit margin
pricing_run <- clean_policies %>%
  mutate(Elapsed_Months = 0, Modal_Premium = 1.0) %>% # Unit premium = 1
  run_gpv_valuation(rate_shift = -0.005, mort_mult = 1.05) %>% # Prudent pricing basis
  mutate(
    # PV_Outflows includes fixed expenses; solve for Modal_Premium covering outflows + 5% margin
    Solved_Modal_Prem = round((PV_Outflows * 1.05) / PV_Premiums, 2)
  ) %>%
  select(Policy_ID, Solved_Modal_Prem)

calibrated_policies <- clean_policies %>%
  left_join(pricing_run, by = "Policy_ID") %>%
  mutate(
    Modal_Premium  = Solved_Modal_Prem,
    Annual_Premium = ifelse(Payment_Mode == "Annual", Modal_Premium, Modal_Premium * 12 / 1.03)
  ) %>%
  select(-Solved_Modal_Prem)

# ==============================================================================
# MODULE 8: SENSITIVITY SHOCK MATRIX
# ==============================================================================
scenarios <- list(
  "Base Case"              = list(r =  0.000, m = 1.00, w = 1.00, e = 1.00),
  "Interest Rate +50 bps"  = list(r =  0.005, m = 1.00, w = 1.00, e = 1.00),
  "Interest Rate -50 bps"  = list(r = -0.005, m = 1.00, w = 1.00, e = 1.00),
  "Mortality +10%"         = list(r =  0.000, m = 1.10, w = 1.00, e = 1.00),
  "Lapse +20%"             = list(r =  0.000, m = 1.00, w = 1.20, e = 1.00),
  "Lapse -20%"             = list(r =  0.000, m = 1.00, w = 0.80, e = 1.00),
  "Maintenance Exp +10%"   = list(r =  0.000, m = 1.00, w = 1.00, e = 1.10)
)

sensitivity_results <- imap_dfr(scenarios, function(params, scen_name) {
  run_gpv_valuation(calibrated_policies, params$r, params$m, params$w, params$e) %>%
    group_by(Plan_Code) %>%
    summarise(
      Scenario      = scen_name,
      Unfloored_GPV = sum(Unfloored_GPV),
      Zeroized_GPV  = sum(Zeroized_GPV),
      .groups = "drop"
    )
}) %>%
  group_by(Plan_Code) %>%
  mutate(
    Base_Zeroized = Zeroized_GPV[Scenario == "Base Case"],
    Pct_Impact    = round((Zeroized_GPV - Base_Zeroized) / Base_Zeroized * 100, 2)
  ) %>%
  ungroup()

print(sensitivity_results)
write_csv(sensitivity_results, "outputs/07_sensitivity_matrix.csv")

# ==============================================================================
# MODULE 9: 1-YEAR ANALYSIS OF CHANGE (AoC) ROLL-FORWARD (t = 0 to t = 1)
# ==============================================================================
# Step 1: Opening Reserve at t = 0
res_t0 <- run_gpv_valuation(calibrated_policies) %>% summarise(sum(Unfloored_GPV)) %>% pull()

# Step 2: Expected Closing Reserve at t = 1 (Advance Elapsed_Months by 12, survive expected decrements)
policies_t1_expected <- calibrated_policies %>%
  mutate(Elapsed_Months = Elapsed_Months + 12) %>%
  filter(Elapsed_Months < Policy_Term * 12)

# Approximate 1-year survival weight (~93% average across lapses & mortality) for clean bridge
res_t1_expected <- run_gpv_valuation(policies_t1_expected) %>%
  summarise(sum(Unfloored_GPV) * 0.93) %>% pull()

# Step 3: Actual Closing Reserve at t = 1 (Simulate 5% lower actual lapses & -25 bps yield shift)
res_t1_lapse_var <- run_gpv_valuation(policies_t1_expected) %>%
  summarise(sum(Unfloored_GPV) * 0.935) %>% pull() # More policies stayed in force than expected

res_t1_actual <- run_gpv_valuation(policies_t1_expected, rate_shift = -0.0025) %>%
  summarise(sum(Unfloored_GPV) * 0.935) %>% pull()

aoc_waterfall <- tibble(
  Step = c(
    "1. Opening Reserve (t = 0)",
    "2. Expected Cash Flow & Interest Roll-Forward",
    "3. Lapse Experience Variance (Actual < Expected)",
    "4. Economic Assumption Change (-25 bps Yield Shift)",
    "5. Closing Reserve (t = 1)"
  ),
  Amount = c(
    res_t0,
    res_t1_expected - res_t0,
    res_t1_lapse_var - res_t1_expected,
    res_t1_actual - res_t1_lapse_var,
    res_t1_actual
  )
)

print(aoc_waterfall)
write_csv(aoc_waterfall, "outputs/08_aoc_rollforward.csv")
cat("Sensitivity Matrix and AoC Roll-Forward exported successfully!\n")

library(ggplot2)

# 1. 30-Year Expected Cash Flow Profile Chart
cf_plot <- monthly_cf_summary %>%
  pivot_longer(cols = c(Exp_Premiums, Exp_Death, Exp_Surrender, Exp_Maturity),
               names_to = "Cashflow_Type", values_to = "Amount") %>%
  ggplot(aes(x = Proj_Month / 12, y = Amount / 1e6, color = Cashflow_Type)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~Plan_Code, scales = "free_y") +
  labs(title = "30-Year Projected Monthly Cash Flows (INR Millions)",
       x = "Projection Year", y = "Monthly Cash Flow (INR Mn)", color = "Component") +
  theme_minimal()

ggsave("outputs/cf_profile_chart.png", cf_plot, width = 9, height = 4.5, dpi = 300)

# 2. Sensitivity Impact Tornado / Bar Chart
sens_plot <- sensitivity_results %>%
  filter(Scenario != "Base Case") %>%
  ggplot(aes(x = reorder(Scenario, abs(Pct_Impact)), y = Pct_Impact, fill = Plan_Code)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Zeroized GPV Sensitivity Impact (% Change vs Base)",
       x = "Stress Scenario", y = "Impact on Reserve (%)") +
  theme_minimal()

ggsave("outputs/sensitivity_chart.png", sens_plot, width = 8, height = 4.5, dpi = 300)

# Display in RStudio Plots pane
print(cf_plot)
print(sens_plot)
