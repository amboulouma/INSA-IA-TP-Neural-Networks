NB. --------------------------------------------------------------------------------
NB. Utilities

load'trig plot numeric stats/base/random'

mp=: +/ . *
mp2=: mp"2 1
cp=: */"1
norm=: [: %: [: +/ *:
mean=: +/ % #
print=: 1!:2&2

gaussian_rand=: dyad define  NB. y random numbers from a gaussian with mu=0 and std=x
  x * normalrand y
)
noisy=: adverb define  NB. 'f noisy' is f with an additional gaussian noise with mu=0 and std=NOISE
  (] + NOISE&gaussian_rand @: #) @: u
)
uniform_rand=: monad define  NB. y random numbers from a uniform distribution between 0 and 1
  ? y $ 0
)
rand_param2=: dyad define  NB. y random parameters
  shift=. {. x [ scale=. {: x
  shift + scale * uniform_rand y
)
rand_param1=: dyad define  NB. y random parameters from a uniform distribution btwn ((-x%2) , x%2)
  ((- -: x),x) rand_param2 y
)
rand_param=: verb define NB. random weights scaled proportionally to the number of input links to a neuron
  (%: 2 % {. y) rand_param1 y 
)

NB. --------------------------------------------------------------------------------
NB. Example functions

lin=: 3: + 2&*
nlin=: verb def '(^y) * cos 2*pi * sin pi * y'
plot_fn=: verb define
  pd 'reset'
  pd 'type line'
  pd (;FN) steps 0 1 100
  pd 'type marker'
  pd 'markers circle'
  pd (; FN noisy) ? 30#0
  pd 'show'
)

NB. --------------------------------------------------------------------------------
NB. dataset init

init_dataset=: verb define
  XP=: uniform_rand NB_POINTS
  YP=: FN noisy XP
  MEAN_YP=: mean YP
  YP=: YP - MEAN_YP  NB. a common practice is to center the dataset with a mean of zero
  XP=: ,. XP         NB. so that the shape of XP be NB_POINTS 1
  YP=: ,. YP
  0
)

NB. --------------------------------------------------------------------------------
NB. neural network core

a=: 0&>.   NB. max(0,x)
da=: >&0   NB. 1 if x is >0, 0 otherwise
dh=: verb def '+: y - YP'

init=: verb define
  prevC=: C [ C=: }: M $ each CINIT
  prevV=: V [ V=: rand_param each 2 <@|.\ >M
)
fwdupd=: verb define
  R=: R , < ((>{:A) mp >y{V) +"1 >y{C
  A=: A , a each {: R
)
fwd=: verb define
  A=: <y
  R=: $0
  fwdupd"0 |.i.#V
  A=: }: A
  >{:R
)
updDV=: verb def 'DV=: DV , < (> y{A) cp (>{:DC)'
bwdupd=: verb define
  updDV - >: y
  DC=: DC , < ((> y{V) mp2 (>{:DC)) * (> y{DA)
)
bwd=: verb define
  DC=: dh each {: R
  DV=: $0
  DA=: |. }: da each R
  bwdupd"0 i.<:#V
  updDV 0
)
fwdbwd=: verb define
  fwd XP
  bwd 0
  a=. alpha % #XP
  C=: (C (- a"_ * +/) each DC) + each (C (*&BETA @ -)each prevC)
  V=: (V (- a"_ * +/) each DV) + each (V (*&BETA @ -)each prevV)
  prevC=: C [ prevV=: V
)

NB. --------------------------------------------------------------------------------
NB. gradient descent

gd=: verb define NB. gradient descent
  nb_iter=:0 [ alpha=: ALPHA [ lstRMSE=: $0
  whilst. nb_iter < MAXITER do.
    fwdbwd 0
    alpha=: alpha * % >: DECAY * nb_iter
    nb_iter=: >: nb_iter
    lstRMSE=: lstRMSE , getRMSE 0
  end.
)

getRMSE=: verb def '%: mean *: (>{:R) - YP' NB. root mean square error

NB. --------------------------------------------------------------------------------
NB. plot the neural network result

plot_nn=: verb define
  pd 'reset'
  pd 'type line'
  pd (; [: , [: fwd ,.) steps 0 1 100
  pd (; FN - MEAN_YP"_) steps 0 1 100
  pd 'type marker'
  pd 'markers circle'
  pd XP;YP
  pd 'show'
)

NB. --------------------------------------------------------------------------------
NB. parameters
setLayers=: verb def 'M=: <"(0) y'
setLayers 1 10 10 1 NB. layers
ALPHA=: 1e_1      NB. gradient descent rate
BETA=: 5          NB. gradient momentum
DECAY=: 1e_7      NB. learning rate decay schedule
MAXITER=: 5000    NB. maximum number of iterations for the gradient descent
CINIT=: 1e_2      NB. constant to initialize the bias
NOISE=: 0.8       NB. standard deviation for the 0-mean gaussian noise
NB_POINTS=: 100   NB. size of the dataset
FN=: nlin

best=: verb define 
  init 0    NB. initialize the weights
  gd 0      NB. learn the weights by gradient descent
  if. bestRMSE > {: lstRMSE do. bestRMSE=: {: bestLstRMSE=: lstRMSE [ bestC=: C [ bestV=: V end.
)

run=: verb define NB. keep the best of y runs and plot the results
  bestRMSE=: 1e6
  best^:y 0
  C=: bestC [ V=: bestV
  plot bestLstRMSE
  plot_nn 0 NB. plot the results
)

eval=: verb define
  init_dataset 0
  fwd XP
  print getRMSE 0
  plot_nn 0
)
