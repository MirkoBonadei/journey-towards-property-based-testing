# Journey towards property based testing
Some experiments of property based testing in Erlang, using [Quiviq](http://www.quviq.com/)'s QuickCheck (mini) for Erlang.

The goal is to have a friendly repository in which collect some useful code to play with Quickcheck and learn as much as possible. I have grabbed the Makefile from [Gianfranco Alongi](https://github.com/Gianfrancoalongi)'s blog post about Erlang and automated testing (you can find the URL in the resources).

### Project structure

The structure of the project is quite straight forward.
- `src`: modules which contain the functions we want to test with `eqc`
- `test`: test modules (the naming standard is `<MODULE_NAME>_eqc.erl`) where we define the properties of our function

### Usage

To check our properties we can use the command `make mod=<MODULE_NAME_eqc> test`.

Here is an example:

```shell
mirko@death-star:~/code/journey-towards-property-based-testing$ make mod=my_lists_eqc test
erlc -o ebin/ -pa lib/eqc-1.0.1/ebin/ src/*.erl test/*.erl
erl -pa ebin/ -pa lib/eqc-1.0.1/ebin/ -eval 'eqc:module(my_lists_eqc)' -eval 'init:stop()'
Erlang/OTP 17 [erts-6.0] [source-07b8f44] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V6.0  (abort with ^G)
1> prop_sum_monotonicity: Starting eqc mini version 1.0.1 (compiled at {{2010,6,13},{11,15,30}})
........................................................................................................................................................................................................
OK, passed 200 tests
prop_sum_identity: ........................................................................................................................................................................................................
OK, passed 200 tests
prop_sum_base_case: ........................................................................................................................................................................................................
OK, passed 200 tests
```

### An example of property:

This is related to the function `my_lists:sum/1`, which takes a `list()` as input and returns the sum of the elements of the list.
The property should be read as: _foreach list *L* of natural numbers, the sum of the elements of the list *L* is equal to the sum of the elements of the element *0* concatenated to the list *L*_.

```erl
prop_sum_identity() ->
  numtests(?NUMBER_OF_GENERATED_TESTS,
    ?FORALL({L}, {list(nat())},
            begin
              L1 = [0|L],
              eqc:equals(my_lists:sum(L), my_lists:sum(L1))
            end)).
```


## Resources
There is not so much material on the web, so that I try to list below some useful resources.

### Readings
- [Quickcheck: A lightweight tool for random testing of Haskell programs](http://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf): paper by John Huges and Koen Claessen
- [Triq: The free quickcheck for Erlang](http://www.javalimit.com/2010/05/triq-the-free-quickcheck-for-erlang.html):
A genuine explanation of how the "clone" of QuickCheck written by Kresten Krab Thorup works
- [Triq - Trifork Quickcheck](https://github.com/krestenkrab/triq)
- [Automated Testing – Bringing out the big guns – Part 1](http://erlcode.wordpress.com/2010/11/10/automated-testing-bringing-out-the-big-guns-part-1/)
- [Automated Testing – Bringing out the big guns – Part 2](https://erlcode.wordpress.com/2010/11/21/automated-testing-%E2%80%93-bringing-out-the-big-guns-%E2%80%93-part-2/)
- [Automated Testing – Bringing out the big guns – Part 3](https://erlcode.wordpress.com/2010/12/05/automated-testing-%E2%80%93-bringing-out-the-big-guns-%E2%80%93-part-3/)
- [An example of Property Based Testing in Erlang](http://jlouisramblings.blogspot.it/2011/12/example-of-property-based-testing-in.html)
- [Notes on Erlang QuickCheck](http://roberto-aloi.com/erlang/notes-on-erlang-quickcheck/)
- [Erlang Quickcheck Mini](https://github.com/rpt/eqcmini)
- [Misadventures with Property-Based TDD: A Lesson Learned](http://www.natpryce.com/articles/000800.html): Some useful advice by Nat Price on how to start with property based testing if you are coming from example based testing
- [Exploring Test-Driven Development with QuickCheck](http://www.natpryce.com/articles/000795.html): Nat Price on TDD with QuickCheck
- [TDD with QuickCheck](http://primitive-automaton.logdown.com/posts/142511/tdd-with-quickcheck): to continue the discourse about TDD and property based testing. (it is also an answer to the previous resource)
- [Property-Based Testing and Verification: a Catalog of Classroom Examples](http://www.cs.ou.edu/~rlpage/SEcollab/rlpIFL2011.pdf)
- [Writing simple-check by Reid Draper](http://reiddraper.com/writing-simple-check/)



### Videos
- [Basho Technologies Hangout 005 - Property Based Testing](https://www.youtube.com/watch?v=D06M8NMJYCw): interesting talk between Tom Santero and Reid Draper
- [Powerful Testing with test.check by Reid Draper at Clojure/West 2014](https://www.youtube.com/watch?v=JMhNINPo__g)
- [Video: Midwest.io 2014 - Property-Based Testing for Better Code by Jessica Kerr](https://www.youtube.com/watch?v=shngiiBfD80)

### Code
- [Basho Repositories](https://github.com/basho): Basho repositories are full of examples (even complex ones) on how to use QuickCheck to test their products
