#lang roulette/example/blog

type Person;
distinct Person Alice, Bob;

fixed Boolean AlwaysTrue = true;
fixed Boolean IsAlice(Person p) = p == Alice;

random Real prob ~ Categorical({0.1 -> 0.5, 0.9 -> 0.5});

random Boolean Smokes(Person p) ~ BooleanDistrib(prob);

random Boolean Healthy(Person p) ~
  if Smokes(p) then BooleanDistrib(0.4)
  else BooleanDistrib(0.9);

//comments 


random Boolean Infected(Person p) ~ BooleanDistrib(prob);

random Boolean ExtraHealthy(Person p) ~
  if (exists Person q Infected(q)) then BooleanDistrib(0.2)
  else BooleanDistrib(0.8);

obs Smokes(Alice) = true;

query (ExtraHealthy(Alice));
