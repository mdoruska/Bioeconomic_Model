## Model with labor market in a loop ##
## Molly Doruska ##
## August 28, 2023 ##

# get working directory
pwd()

# set new working directory
cd("~\\BioeconomicModelReplication")

# Load in required packages #
using NLopt
using Plots
using LinearAlgebra
using Distributions
using Random
using DifferentialEquations
using DelimitedFiles

# set up functions of households problem
function utility(x::Vector, grad::Vector)
    if length(grad) > 0
        grad[1] =  (-(θf*((x[1]^(θf - 1))*(x[2]^θg)*(((e^(S1/(S1+I1)))*(x[1]^hf))^θh)*(x[6]^θl))) -
                    ((x[1]^θf)*(x[2]^θg)*(x[6]^θl)*θh*((e^(S1/(S1+I1)))*(x[1]^hf))^(θh-1)*hf*(x[1]^(hf-1))))
        grad[2] = -(θg*((x[1]^θf)*(x[2]^(θg - 1))*(((e^(S1/(S1+I1)))*(x[1]^hf))^θh)*(x[6]^θl)))
        grad[3] = 0
        grad[4] = 0
        grad[5] = 0
        grad[6] = -(θl*((x[1]^θf)*(x[2]^θg)*(((e^(S1/(S1+I1)))*(x[1]^hf))^θh)*(x[6]^(θl-1))))
        grad[7] = 0
        grad[8] = 0
        grad[9] = 0
    end
    return -((x[1]^θf)*(x[2]^θg)*(((e^(S1/(S1+I1)))*(x[1]^hf))^θh)*(x[6]^θl))
end

function laborconstraint(x::Vector, grad::Vector, el)
    if length(grad) > 0
        grad[1] = 0
        grad[2] = 0
        grad[3] = 1
        grad[4] = 1
        grad[5] = 0
        grad[6] = 1
        grad[7] = 0
        grad[8] = 0
        grad[9] = 1
    end
    x[3] + x[4] + x[6] + x[9] - el
end

# create function with differential equations
function parameterized_disease_ecology(du,u,p,t)
    N,S1,I1,M,S2,I2,P = u
    r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv = p
    du[1] = p[22] + p[1]*u[1]*(1-(u[1]/p[2])) - (p[23]/365)
    du[2] = p[3] - (p[4]*u[7]*u[2]/(1+p[5]*u[7])) - p[6]*u[2] + p[7]*u[3]
    du[3] = (p[4]*u[7]*u[2]/(1+p[5]*u[7])) - (p[6] + p[8] + p[7])*u[3]
    du[4] = p[9]*p[10]*u[3] - p[11]*u[4]
    du[5] = p[12] - (p[13]*u[4]*u[5]/(p[14] + p[15]*u[4]^2)) - (p[16] + p[17]*(p[2]-u[1]))*u[5]
    du[6] = (p[13]*u[4]*u[5]/(p[14] + p[15]*u[4]^2)) - (p[16] + p[18] + p[17]*(p[2]-u[1]))*u[6]
    du[7] = p[19]*u[6] - (p[20] + p[21])*u[7]
end

# household model parameters
θf = 0.55 # coefficient on food in the utility funciton
θg = 0.3 # coefficient on the general household good in the utility funciton
θh = 0.1 # coefficient on health in the utility function
θl = 0.05 # coefficient on leisure in teh utility funciont
pricef = 1 # originally 290 FCFA, normalized price of food
priceg = 1.72  #originally 500 FCFA, normalized price of the household good
priceu = 300/290 # originally 270 FCFA, normalized price of fertilizer
δ = 0.6 # amount of vegetation that becomes compost
alphad = 0.4 # Coefficient on land in food production
alphal = 0.5 # Coefficient on labor in food production
alphau = 0.05 # Coefficient on fertilizer in food production
alphav = 0.05 # Coefficient on vegetation in food production
r_i = 0.3 # substitution parameter
betav = 14.4942 # Coefficient for harvesting vegetation
gamma1 = 0.2595 # exponent on labor in harvesting vegetation
hf = 0.000384 # Coefficeint on food consumption in health status function
e = ℯ
labor_scale_f = 150 # scale for labor shares to days
labor_scale_v = 150 # scale for labor shares to days

# labor and land endowment
labor_e = 7 # household members
land = 0.5 # household land

# fertilizer impact on vegetation growth
ρ = 0.01

# disease ecology parameters
# parameterize the problem
r = 0.05 # natural growth rate of vegetation
K = 28.9065 # estimated carrying capacity of vegetation
n0 = 0.01 # positive intercept for vegetation
qv = 24.15 # estimated desired to be removed
Λ1 = 0 # no human additions 8000
β1 = 1.766e-08 # contact between cercariae and humans 0.406e-08
α1 = 0.8e-08 # saturation coefficeint for cercarial infectivity 0.3e-08
μ1 = 0  # no human deaths 0.0000384
η = 0.0068 # chance of cure of human infection 0.0068
δ1 = 0 # no human deaths from infection
k = 300 # portion of eggs released into the environment per infected human host 300
γ1 = 50 # hatching rate of miracidia 75
μ3 = 2.5 # miracidial mortality rate 4.5
Λ2 = 100 # snail recruitment 200
β2 = 0.615 # probability of snail infection from miracidia 0.615
M0 = 1.00e06 # contact rate between micacidia and snails 1.00e06
ϵ = 0.3 # saturation coefficeint for miracidial infections 0.3
μ2 = 0.008 # natural snail mortality rate 0.000569 0.00569
θ = 0.02842 # eilimnation rate of snails per kg of vegetation (this is how vegetation enters into this model)
δ2 = 0.0004012 # snail mortality rate due to infection
γ2 = 2.6 # cercarial emergence rate
μ4 = 0.004 # cercarial mortality rate
τ = 0 # elimination rate of cercarie (assume no cercarie elimiation)

p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]

# starting values for disease ecology model
N_0 = 28.9065 # starting value of vegetation
I1_0 = 2.5 # starting value infected humans
S1_0 = 7.5 # starting value susceptible humans
M_0 = 15000 # starting value miracida
S2_0 = 12300 # starting value susceptible snails
I2_0 = 200 # starting value infected snails
P_0 = 130000 # starting value cercarie

# years
years = 20; # simulation time

# simulations
simul = 1000; # number of simulations 

# initial infection draw
pr = 0.25;

# creat time period vector
time = 1:years ;

# set initial values of S1 and I1
S1 = S1_0 ;
I1 = I1_0 ;

# create timespan for differential equations simulation
tspan = (0,365.0)

# no labor market - wage zero and no value to hired labor
wage = 0

function budgetconstraint_nolab(x::Vector, grad::Vector, pf, pg, pu, w, qd, ad, al, au, av, r, bv, γ1)
    if length(grad) > 0
        grad[1] = pf
        grad[2] = pg
        grad[3] = -(pf*(1/r)*((ad*(qd^r) + al*((labor_scale_f*(x[3] + 0*x[7]))^r) + au*((qd*x[5])^r) + av*(δ*(bv*(labor_scale_v*(x[4] + 0*x[8]))^γ1))^r)^((1\r) - 1))*(al*r*(labor_scale_f*(x[3] + 0*x[7]))^(r-1)))*labor_scale_f
        grad[4] = -(pf*(1/r)*((ad*(qd^r) + al*((labor_scale_f*(x[3] + 0*x[7]))^r) + au*((qd*x[5])^r) + av*(δ*(bv*(labor_scale_v*(x[4] + 0*x[8]))^γ1))^r)^((1\r) - 1))*(av*r*(δ*(bv*(labor_scale_f*(x[4] + 0*x[8]))^γ1))^(r-1))*(δ*bv*γ1*(labor_scale_v*(x[4] + 0*x[8]))^(γ1-1)))*labor_scale_v
        grad[5] = pu - (pf*(1/r)*((ad*(qd^r) + al*((labor_scale_f*(x[3] + 0*x[7]))^r) + au*((qd*x[5])^r) + av*(δ*(bv*(labor_scale_v*(x[4] + 0*x[8]))^γ1))^r)^((1\r) - 1))*(au*r*(qd*x[5])^(r-1))*qd)
        grad[6] = 0
        grad[7] = 0 + w
        grad[8] = 0 + w
        grad[9] = -w
    end
    -(pf*((ad*(qd^r) + al*((labor_scale_f*(x[3] + 0*x[7]))^r) + au*((qd*x[5])^r) + av*(δ*(bv*(labor_scale_v*(x[4] + 0*x[8]))^γ1))^r)^(1/r))) - w*x[9] +
        pf*x[1] + pg*x[2] + w*(x[7] + x[8]) + pu*x[5]
end

# check that optimizaiton is working correctly
opt = Opt(:LN_COBYLA, 9)
opt.min_objective = utility
opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
opt.upper_bounds = [Inf, Inf, labor_e, labor_e, Inf, labor_e, 0, 0, 0]
opt.xtol_rel = 1e-6
inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labor_e), 1e-8)
wagestartvalues = Vector{Float64}([40, 3, 2, 0, 10, 0.5, 0, 0, 0])
(optf, optx, ret) = optimize(opt, wagestartvalues)

# check that differential equations simulation works correctly
u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0]
prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
simul1 = solve(prob1)

# analyze solution
oneyear = simul1(365.0)

# optimization simulation loop function
function health_opt_simul_nolab(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 2.5 # starting value infected humans
                S1_0 = 7.5 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 2.5
                presus[j] = 7.5
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

# Saint Louis 25th percentile
land = 0.5
saintlouisland25_nolab = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand25_pu300t.csv", saintlouisland25_nolab, ',')

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolab = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pu300t.csv", saintlouisland50_nolab, ',')

# Saint Louis 75th percentile
land = 5.5
saintlouisland75_nolab = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand75_pu300t.csv", saintlouisland75_nolab, ',')

# price sensitivity analysis - household good
priceg = (300/290)

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolab_p300 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_p300.csv", saintlouisland50_nolab_p300, ',')

priceg = (700/290)

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolab_p700 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_p700.csv", saintlouisland50_nolab_p700, ',')

# price sensitivity analysis - urea fertilizer
priceg = (500/290)
priceu = (200/290)

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolab_pu200 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pu200.csv", saintlouisland50_nolab_pu200, ',')

priceu = (500/290)

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolab_pu500 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pu500.csv", saintlouisland50_nolab_pu500, ',')

# no labor market and no vegetation harvest
bv = 0
priceu = (300/290)
priceg = (500/290)

function health_opt_simul_noharvestnolab(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 2.5 # starting value infected humans
                S1_0 = 7.5 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 2.5
                presus[j] = 7.5
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, 0, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = vegload[j] +  ρ*fert[j]*land*vegload[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, 0, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] - vegprod[j] +  ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1
                if vegload[j] < 0
                    veglimit = 0
                else
                    veglimit = 0
                end

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, 0, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

wagestartvalues = Vector{Float64}([40, 3, 2, 0, 10, 0.5, 0, 0, 0])

# Saint Louis 25th percentile
land = 0.5
saintlouisland25_nhnolab = health_opt_simul_noharvestnolab(years, simul, wagestartvalues)
writedlm("SaintLouisLaborLand25_nhnolab_pu300.csv", saintlouisland25_nhnolab, ',')

# Saint Louis 50th percentile
land = 2
saintlouisland50_nhnolab = health_opt_simul_noharvestnolab(years, simul, wagestartvalues)
writedlm("SaintLouisLaborLand50_nhnolab_pu300.csv", saintlouisland50_nhnolab, ',')

# Saint Louis 75th percentile
land = 5.5
saintlouisland75_nhnolab = health_opt_simul_noharvestnolab(years, simul, wagestartvalues)
writedlm("SaintLouisLaborLand75_nhnolab_pu300.csv", saintlouisland75_nhnolab, ',')

# fertilizer feedback sensitivity analysis
betav = 14.4942
ρ = 0

# Saint Louis 25th percentile
land = 0.5
saintlouisland25_nolabnofert = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand25_nofert_pu300.csv", saintlouisland25_nolabnofert, ',')

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolabnofert = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_nofert_pu300.csv", saintlouisland50_nolabnofert, ',')

# Saint Louis 75th percentile
land = 5.5
saintlouisland75_nolabnofert = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand75_nofert_pu300.csv", saintlouisland75_nolabnofert, ',')

ρ = 0.005

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolabrho005 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_rho005.csv", saintlouisland50_nolabrho005, ',')

ρ = 0.02

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolabrho02 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_rho02.csv", saintlouisland50_nolabrho02, ',')

# recolonization rate sensitivity analysis
ρ = 0.01
n0 = 0.005
p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolabn0005 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_n0005.csv", saintlouisland50_nolabn0005, ',')

n0 = 0.02
p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolabn002 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_n002.csv", saintlouisland50_nolabn002, ',')

# vegetation growth rate sensitivity analysis
n0 = 0.01
r = 0.075
p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolabr075 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_r075.csv", saintlouisland50_nolabr075, ',')

r = 0.025
p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]

# Saint Louis 50th percentile
land = 2
saintlouisland50_nolabr025 = health_opt_simul_nolab(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_r025.csv", saintlouisland50_nolabr025, ',')

# optimal fertilizer use at different levels of schisto starting infection rate
r = 0.05
p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]

# infection rate 10%
I1_0 = 1 # starting value infected humans
S1_0 = 9 # starting value susceptible humans

pr = 0.1;

function health_opt_simul_nolab_pr01(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 1 # starting value infected humans
                S1_0 = 9 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 1
                presus[j] = 9
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

land = 2
saintlouisland50_nolab_pr01 = health_opt_simul_nolab_pr01(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pr01.csv", saintlouisland50_nolab_pr01, ',')

# infection rate 20%
I1_0 = 2 # starting value infected humans
S1_0 = 8 # starting value susceptible humans

pr = 0.2;

function health_opt_simul_nolab_pr02(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 2 # starting value infected humans
                S1_0 = 8 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 2
                presus[j] = 8
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

land = 2
saintlouisland50_nolab_pr02 = health_opt_simul_nolab_pr02(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pr02.csv", saintlouisland50_nolab_pr02, ',')

# infection rate 30%
I1_0 = 3 # starting value infected humans
S1_0 = 7 # starting value susceptible humans

pr = 0.3;

function health_opt_simul_nolab_pr03(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 3 # starting value infected humans
                S1_0 = 7 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 3
                presus[j] = 7
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

land = 2
saintlouisland50_nolab_pr03 = health_opt_simul_nolab_pr03(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pr03.csv", saintlouisland50_nolab_pr03, ',')

# infection rate 40%
I1_0 = 4 # starting value infected humans
S1_0 = 6 # starting value susceptible humans

pr = 0.4;

function health_opt_simul_nolab_pr04(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 4 # starting value infected humans
                S1_0 = 6 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 4
                presus[j] = 6
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

land = 2
saintlouisland50_nolab_pr04 = health_opt_simul_nolab_pr04(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pr04.csv", saintlouisland50_nolab_pr04, ',')

# infection rate 50%
I1_0 = 5 # starting value infected humans
S1_0 = 5 # starting value susceptible humans

pr = 0.5;

function health_opt_simul_nolab_pr05(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 5 # starting value infected humans
                S1_0 = 5 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 5
                presus[j] = 5
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

land = 2
saintlouisland50_nolab_pr05 = health_opt_simul_nolab_pr05(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pr05.csv", saintlouisland50_nolab_pr05, ',')

# infection rate 60%
I1_0 = 6 # starting value infected humans
S1_0 = 4 # starting value susceptible humans

pr = 0.6;

function health_opt_simul_nolab_pr06(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 6 # starting value infected humans
                S1_0 = 4 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 6
                presus[j] = 4
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

land = 2
saintlouisland50_nolab_pr06 = health_opt_simul_nolab_pr06(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pr06.csv", saintlouisland50_nolab_pr06, ',')

# infection rate 70%
I1_0 = 7 # starting value infected humans
S1_0 = 3 # starting value susceptible humans

pr = 0.7;

function health_opt_simul_nolab_pr07(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 7 # starting value infected humans
                S1_0 = 3 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 7
                presus[j] = 3
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

land = 2
saintlouisland50_nolab_pr07 = health_opt_simul_nolab_pr07(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pr07.csv", saintlouisland50_nolab_pr07, ',')

# infection rate 80%
I1_0 = 8 # starting value infected humans
S1_0 = 2 # starting value susceptible humans

pr = 0.8;

function health_opt_simul_nolab_pr08(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 8 # starting value infected humans
                S1_0 = 2 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 8
                presus[j] = 2
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

land = 2
saintlouisland50_nolab_pr08 = health_opt_simul_nolab_pr08(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pr08.csv", saintlouisland50_nolab_pr08, ',')

# infection rate 90%
I1_0 = 9 # starting value infected humans
S1_0 = 1 # starting value susceptible humans

pr = 0.9;

function health_opt_simul_nolab_pr09(n::Integer,s::Integer,start_vals::Vector)
    # initialize this period results vector
    thisperiodresults= zeros(1, 24)
    results = zeros(1,24)
    # outside loop of simulation replications
    for i=1:s
        # initialize all vectors for storing results
        time = 1:n
        consumptionfood = Vector{Float64}(undef, years)
        consumptionhhgood = Vector{Float64}(undef, years)
        healthstatus = Vector{Float64}(undef, years)
        foodlabor = Vector{Float64}(undef, years)
        vegprod = Vector{Float64}(undef, years)
        fert = Vector{Float64}(undef, years)
        veglabor = Vector{Float64}(undef, years)
        foodprod = Vector{Float64}(undef, years)
        leisure = Vector{Float64}(undef, years)
        hiredfarm = Vector{Float64}(undef, years)
        hiredveg = Vector{Float64}(undef, years)
        marketlabor = Vector{Float64}(undef, years)
        foodlabortotal = Vector{Float64}(undef, years)
        veglabortotal = Vector{Float64}(undef, years)
        vegload = Vector{Float64}(undef, years)
        infected = Vector{Float64}(undef, years)
        susceptible = Vector{Float64}(undef, years)
        miracida = Vector{Float64}(undef, years)
        sussnails = Vector{Float64}(undef, years)
        infsnails = Vector{Float64}(undef, years)
        cercariae = Vector{Float64}(undef, years)
        preinf = Vector{Float64}(undef, years)
        presus = Vector{Float64}(undef, years)
        # inside loop of each time period run
        for j = 1:n
            # different first period
            if j == 1
                # load in initial state values
                N_0 = 28.9065 # starting value of vegetation
                I1_0 = 9 # starting value infected humans
                S1_0 = 1 # starting value susceptible humans
                M_0 = 15000 # starting value miracida
                S2_0 = 12300 # starting value susceptible snails
                I2_0 = 200 # starting value infected snails
                P_0 = 130000 # starting value cercarie
                vegload[j] = N_0
                miracida[j] = M_0
                sussnails[j] = S2_0
                infsnails[j] = S1_0
                cercariae[j] = P_0
                preinf[j] = 9
                presus[j] = 1
                pr = preinf[j] / (preinf[j] + presus[j])

                # random infection draws of household
                dist = Bernoulli(pr)
                infect = rand(dist, 10)
                I1 = count(infect)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            elseif 1 < j < n
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1

                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

                # simulate disesae ecology model one period forward
                N_0 = max(0,vegload[j] + ρ*fert[j]*land*vegload[j])
                M_0 = miracida[j]
                S2_0 = sussnails[j]
                I2_0 = infsnails[j]
                P_0 = cercariae[j]
                u0 = [N_0,S1,I1,M_0,S2_0,I2_0,P_0] # initail state vector
                qv = vegprod[j]
                p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ,n0,qv]
                prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
                simul1 = solve(prob1)
                oneyear = simul1(365.0)

                # store one year simulation resutls as next period's state
                vegload[j+1] = oneyear[1]
                miracida[j+1] = oneyear[4]
                sussnails[j+1] = oneyear[5]
                infsnails[j+1] = oneyear[6]
                cercariae[j+1] = oneyear[7]
                preinf[j+1] = oneyear[3]
                presus[j+1] = oneyear[2]

            else
                # new infection draw value
                pr = preinf[j] / (presus[j] + preinf[j])
                if pr > 1 || pr < 0
                    break
                end

                # random infection draws of household
                dist = Bernoulli(pr)
                I1 = Int(infected[j-1])
                S1 = Int(susceptible[j-1])
                infect = rand(dist, S1)
                stillinf = Bernoulli(0.75)
                stillinf2 = rand(stillinf, I1)
                I1 = count(infect) + count(stillinf2)
                S1 = 10 - I1
                infected[j] = I1
                susceptible[j] = S1
                pcinf = (I1 / (S1 + I1))
                labore = S1 + 0.5*I1


                # household solves optimization problem
                opt = Opt(:LN_COBYLA, 9)
                opt.min_objective = utility
                opt.lower_bounds = [0, 0, 0, 0, 0, 0, 0, 0, 0]
                opt.upper_bounds = [Inf, Inf, labore, labore, Inf, labore, 0, 0, 0]
                opt.xtol_rel = 1e-6
                inequality_constraint!(opt, (x, g) -> budgetconstraint_nolab(x, g, pricef, priceg, priceu, wage, land, alphad, alphal, alphau, alphav, r_i, betav, gamma1), 1e-8)
                equality_constraint!(opt, (x, g) -> laborconstraint(x, g, labore), 1e-8)
                startvalues = start_vals
                (optf, optx, ret) = optimize(opt, startvalues)

                # store optimal values
                consumptionfood[j] = optx[1]
                consumptionhhgood[j] = optx[2]
                healthstatus[j] = (((e^(S1/(S1+I1)))*(optx[1]^hf))^θh)
                foodprod[j] = ((alphad*(land^r_i) + alphal*((labor_scale_f*(optx[3] + 0*optx[7]))^r_i) + alphau*(optx[5]^r_i) + alphav*(δ*(betav*(labor_scale_v*(optx[4] + 0*optx[8]))^gamma1))^r_i)^(1/r_i))
                vegprod[j] = (betav*(labor_scale_v*(optx[4] + optx[8]))^gamma1)
                foodlabor[j] = optx[3]
                veglabor[j] = optx[4]
                fert[j] = optx[5]
                leisure[j] = optx[6]
                hiredfarm[j] = optx[7]
                hiredveg[j] = optx[8]
                marketlabor[j] = optx[9]
                foodlabortotal[j] = optx[3] + optx[7]
                veglabortotal[j] = optx[4] + optx[8]

            end
            thisperiodresults = hcat(time, vegload, infected, susceptible, miracida, sussnails, infsnails, cercariae, preinf, presus, consumptionfood, consumptionhhgood, healthstatus, foodlabor, vegprod, fert, veglabor, foodprod, leisure, hiredfarm, hiredveg, marketlabor, foodlabortotal, veglabortotal)
        end
        results = vcat(results, thisperiodresults)
        println("Simulation Number $(i)")
    end
    return results
end

land = 2
saintlouisland50_nolab_pr09 = health_opt_simul_nolab_pr09(years, simul, wagestartvalues)
writedlm("SaintLouisNoLaborLand50_pr09.csv", saintlouisland50_nolab_pr09, ',')
