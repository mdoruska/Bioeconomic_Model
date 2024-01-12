## simulation of differential equations ##
## Molly Doruska ##
## Date last modified:January 10, 2023 ##
## work starting with Gao et al 2011 ##
## January update: add in continuous fertilizer harvest ##

# get working directory
pwd()

# set new working directory
cd("~\\BioeconomicModelResults")

# Load in required packages
using Plots
using DifferentialEquations

# create function with differential equations
function parameterized_disease_ecology(du,u,p,t)
    N,S1,I1,M,S2,I2,P = u
    r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ = p
    du[1] = p[1]*u[1]*(1-(u[1]/p[2]))
    du[2] = p[3] - (p[4]*u[7]*u[2]/(1+p[5]*u[7])) - p[6]*u[2] + p[7]*u[3]
    du[3] = (p[4]*u[7]*u[2]/(1+p[5]*u[7])) - (p[6] + p[8] + p[7])*u[3]
    du[4] = p[9]*p[10]*u[3] - p[11]*u[4]
    du[5] = p[12] - (p[13]*u[4]*u[5]/(p[14] + p[15]*u[4]^2)) - (p[16] + p[17]*(p[2]-u[1]))*u[5]
    du[6] = (p[13]*u[4]*u[5]/(p[14] + p[15]*u[4]^2)) - (p[16] + p[18] + p[17]*(p[2]-u[1]))*u[6]
    du[7] = p[19]*u[6] - (p[20] + p[21])*u[7]
end

# starting values
N_0 = 28.9065 # starting value of vegetation
I1_0 = 2.5 # starting value infected humans
S1_0 = 7.5 # starting value susceptible humans
M_0 = 15000 # starting value miracida
S2_0 = 12300 # starting value susceptible snails
I2_0 = 200 # starting value infected snails
P_0 = 130000 # starting value cercarie

u0 = [N_0,S1_0,I1_0,M_0,S2_0,I2_0,P_0] # initail state vector

# parameterize the problem
r = 0.05 # natural growth rate of vegetation
K = 28.9065 # estimated carrying capacity of vegetation
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

p = [r,K,Λ1,β1,α1,μ1,η,δ1,k,γ1,μ3,Λ2,β2,M0,ϵ,μ2,θ,δ2,γ2,μ4,τ]

# time span
tspan = (0.0,1825.0)

# solve differntial equations
prob1 = ODEProblem(parameterized_disease_ecology,u0,tspan,p)
simul1 = solve(prob1)

# analyze solution
fiveyear = simul1(1825.0)

# create plots of paths of each population
# susceptible humans
plot_sushumans_simul1 = Plots.plot(simul1, vars=(0,2),
    xaxis="Time (days)", yaxis="Susceptible Humans", ylim = (0,10),
    label="S1(t)",linewidth=1, color=:green, guidefontsize=5,
    tickfontsize=3, legendfontsize=4)

# infected humans
plot_infhumans_simul1 = Plots.plot(simul1, vars=(0,3),
    xaxis="Time (days)", yaxis="Infected Humans", ylim = (0,10),
    label="I1(t)",linewidth=1, color=:red, guidefontsize=5,
    tickfontsize=3, legendfontsize=4)

# miracidia
plot_miracidia_simul1 = Plots.plot(simul1, vars=(0,4),
    xaxis="Time (days)", yaxis="Miracidia", ylim = (14000,16000),
    label="M(t)",linewidth=1, color=:purple, guidefontsize=5,
    tickfontsize=3, legendfontsize=4)

# susceptible snails
plot_sussnails_simul1 = Plots.plot(simul1, vars=(0,5),
    xaxis="Time (days)", yaxis="Susceptible Snails", ylim = (11000, 13000),
    label="S2(t)",linewidth=1, color=:green, guidefontsize=5,
    tickfontsize=3, legendfontsize=4)

# infected snails
plot_infsnails_simul1 = Plots.plot(simul1, vars=(0,6),
    xaxis="Time (days)", yaxis="Infected Snails", ylim = (50,300),
    label="I2(t)",linewidth=1, color=:red, guidefontsize=5,
    tickfontsize=3, legendfontsize=4)

# cercariae
plot_cercariae_simul1 = Plots.plot(simul1, vars=(0,7),
    xaxis="Time (days)", yaxis="Cercariae", ylim = (115000,140000),
    label="P(t)",linewidth=1, color=:orange, guidefontsize=5,
    tickfontsize=3, legendfontsize=4)

combined_plot = plot(plot_cercariae_simul1, plot_miracidia_simul1, plot_infsnails_simul1,
    plot_sussnails_simul1, plot_infhumans_simul1, plot_sushumans_simul1,
    layout=(3,2), title=["A" "B" "C" "D" "E" "F"], titleloc = :left,
    titlefontsize=7)
savefig(combined_plot, "Figure_S4.svg")
