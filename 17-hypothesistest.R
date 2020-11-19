library(tidyverse)
input <- tribble(
  ~var,	~cat,	~population,	~sample,
  "a",	"a",	4915123,	1784,
  "a",	"b",	4654345,	1428,
  "b",	"a",	2140679,	580,
  "b",	"b",	2447544,	708,
  "b",	"c",	2216258,	730,
  "b",	"d",	1552849,	715,
  "b",	"e",	1212138,	479,
  "c",	"a",	696911,	133,
  "c",	"b",	729615,	218,
  "c",	"c",	714153,	229,
  "c",	"d",	740731,	223,
  "c",	"e",	866508,	261,
  "d",	"a",	975968,	447,
  "d",	"b",	8436399,	2765,
  "e",	"a",	90949,	58,
  "e",	"b",	2798269,	2526,
  "f",	"a",	8619190,	2121,
  "f",	"b",	793177,	213,
  "g",	"a",	2648106,	975,
  "g",	"b",	2578389,	781,
  "g",	"c",	4342973,	1439,
)
input %>% 
  select(-cat) %>% 
  group_by(var) %>% 
  nest() %>% 
  mutate(data=map(.x = data,.f = as.matrix)) %>% 
  mutate(data=map(.x = data,.f = chisq.test)) %>% 
  mutate(result=map(.x = data,.f = broom::tidy)) %>% 
  unnest(result) %>% 
  mutate(p.value=round(p.value,10))
