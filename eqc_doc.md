# Erlang Quickcheck unofficial documentation

This is my attempt to write a documentation to make Erlang Quickcheck a little bit less difficult to grasp.
This is an **unofficial documentation**, so bear in mind that the most reliable source is still [Quiviq official site](http://www.quviq.com/products/erlang-quickcheck/)

## How to write your generators
Quickcheck offers lots of generators out of the box:

**TODO: list them here**

But in some cases you need to define a new one, for example:

_"We need a generator for newest erlang data type that is **maps**"_.

**TODO: write who to use this is a good English :-)**

```erlang
thing() ->
  ?LET({Weight, Value}, {nat(), nat()}, #{ w => Weight, v => Value}).
```

```erlang
1> eqc_gen:sample(knapsack_eqc:thing()).
#{v => 10,w => 4}
#{v => 10,w => 2}
#{v => 9,w => 10}
#{v => 0,w => 3}
#{v => 10,w => 11}
#{v => 3,w => 7}
#{v => 0,w => 8}
#{v => 0,w => 5}
#{v => 13,w => 14}
#{v => 2,w => 1}
#{v => 11,w => 20}
ok
```
