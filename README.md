### Journey towards property based testing
Some experiments on property based testing in Erlang, using [Trifork QuickCheck](https://github.com/krestenkrab/triq), I will refer to it as `Triq` henceforth.

The point is to explore _property based testing_ coming from some years of _example based testing_ and learn to write properties instead of examples in order to understand the impacts it has on the "mainstream TDD". For "mainstream TDD" I mean TDD with example based testing, to me the two things (TDD and example based testing) are not related but the majority of the literature and the practice in the real world seems to bind TDD to example based testing. So let's see what comes out from this experiment. :-)

#### Project structure

The structure of the project is quite straightforward.
- `src`: modules which contain the functions we want to test with `Triq`
- `test`: test modules (the naming standard is `<MODULE_NAME>_tests.erl`) where we define the properties of our functions

#### Build and Check properties
The project is built with `rebar`, compile code and test properties is really easy:

```shell
mirko@death-star:~/code/journey-towards-property-based-testing$ ./rebar compile qc
==> triq (compile)
==> eunit_formatters (compile)
==> journey-towards-property-based-testing (compile)
==> journey-towards-property-based-testing (qc)
NOTICE: Using experimental 'qc' command
Testing my_lists_tests:prop_sum_monotonicity/0
....................................................................................................
Ran 100 tests
Testing my_lists_tests:prop_sum_identity/0
....................................................................................................
Ran 100 tests
Testing my_lists_tests:prop_sum_base_case/0
....................................................................................................
Ran 100 tests
```


#### An example of property:

This is related to the function `my_lists:sum/1`, which takes a `list()` as input and returns the sum of the elements of the list.
The property should be read as:

_foreach list **L** of natural numbers, the sum of the elements of the list **L** is equal to the sum of the elements of the element **0** concatenated to the list **L**_.

```erlang
prop_sum_identity() ->
  ?FORALL({L}, {list(nat())},
          begin
            L1 = [0|L],
            eqc:equals(my_lists:sum(L), my_lists:sum(L1))
          end).
```


#### Resources
There is not so much material on the web, so that I try to list below some useful resources.

##### Readings
- [Quickcheck: A lightweight tool for random testing of Haskell programs](http://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf): paper by John Huges and Koen Claessen
- [Triq: The free quickcheck for Erlang](http://www.javalimit.com/2010/05/triq-the-free-quickcheck-for-erlang.html):
A genuine explanation of how the "clone" of QuickCheck written by Kresten Krab Thorup works
- [Automated Testing – Bringing out the big guns – Part 1](http://erlcode.wordpress.com/2010/11/10/automated-testing-bringing-out-the-big-guns-part-1/)
- [Automated Testing – Bringing out the big guns – Part 2](https://erlcode.wordpress.com/2010/11/21/automated-testing-%E2%80%93-bringing-out-the-big-guns-%E2%80%93-part-2/)
- [Automated Testing – Bringing out the big guns – Part 3](https://erlcode.wordpress.com/2010/12/05/automated-testing-%E2%80%93-bringing-out-the-big-guns-%E2%80%93-part-3/)
- [An example of Property Based Testing in Erlang](http://jlouisramblings.blogspot.it/2011/12/example-of-property-based-testing-in.html)
- [Notes on Erlang QuickCheck](http://roberto-aloi.com/erlang/notes-on-erlang-quickcheck/)
- [Misadventures with Property-Based TDD: A Lesson Learned](http://www.natpryce.com/articles/000800.html): Some useful advice by Nat Price on how to start with property based testing if you are coming from example based testing
- [Exploring Test-Driven Development with QuickCheck](http://www.natpryce.com/articles/000795.html): Nat Price on TDD with QuickCheck
- [TDD with QuickCheck](http://primitive-automaton.logdown.com/posts/142511/tdd-with-quickcheck): to continue the discourse about TDD and property based testing. (it is also an answer to the previous resource)
- [Property-Based Testing and Verification: a Catalog of Classroom Examples](http://www.cs.ou.edu/~rlpage/SEcollab/rlpIFL2011.pdf)
- [Writing simple-check by Reid Draper](http://reiddraper.com/writing-simple-check/)


##### Videos
- [Using open-source Trifork QuickCheck to test Erjang](http://vimeo.com/17102985): Kresten Krab Thorup explains how Triq was used to test Erjang
- [Basho Technologies Hangout 005 - Property Based Testing](https://www.youtube.com/watch?v=D06M8NMJYCw): interesting talk between Tom Santero and Reid Draper
- [Powerful Testing with test.check by Reid Draper at Clojure/West 2014](https://www.youtube.com/watch?v=JMhNINPo__g)
- [Video: Midwest.io 2014 - Property-Based Testing for Better Code by Jessica Kerr](https://www.youtube.com/watch?v=shngiiBfD80)

##### Code
- [Triq - Trifork Quickcheck](https://github.com/krestenkrab/triq): the code of the open source Trifork QuickCheck, truly reccomended to understand how it works
- [Basho Repositories](https://github.com/basho): Basho repositories are full of examples (even complex ones) on how to use QuickCheck to test their products
- [Erlang Quickcheck Mini](https://github.com/rpt/eqcmini): some examples with Quiviq Erlang QuickCheck Mini
