# Journey towards property based testing
Some experiments of property based testing in Erlang, using [Quiviq](http://www.quviq.com/)'s QuickCheck (mini) for Erlang.

The goal is to have a friendly repository in which collect some useful code to play with Quickcheck and learn as much as possible. I have grabbed the Makefile from [Gianfranco Alongi](https://github.com/Gianfrancoalongi)'s blog post about Erlang and automated testing (you can find the URL in the resources).

The structure of the project is quite straight forward. There are two main directories `src` and `test`. Properties are written in the `test` directory with the following naming convention `module_eqc` and the code under test will be located under `src`. So that code is organized in couple of files, the module itself and the properties of the module.

To check our properties we can use the command `make mod=property_module_name test`. Here are some examples:

```shell
mirko@death-star:~/code/journey-towards-property-based-testing$ make mod=lists_eqc test
erlc -o ebin/ -pa lib/eqc-1.0.1/ebin/ src/*.erl test/*.erl
erl -pa ebin/ -pa lib/eqc-1.0.1/ebin/ -eval 'eqc:module(lists_eqc)' -eval 'init:stop()'
Erlang/OTP 17 [erts-6.0] [source-07b8f44] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V6.0  (abort with ^G)
1> prop_delete: Starting eqc mini version 1.0.1 (compiled at {{2010,6,13},{11,15,30}})
.....................................Failed! After 38 tests.
{-8,[-8,-8,12]}
Shrinking.(1 times)
{-8,[-8,-8]}

mirko@death-star:~/code/journey-towards-property-based-testing$ make mod=lists_eqc test
erlc -o ebin/ -pa lib/eqc-1.0.1/ebin/ src/*.erl test/*.erl
erl -pa ebin/ -pa lib/eqc-1.0.1/ebin/ -eval 'eqc:module(lists_eqc)' -eval 'init:stop()'
Erlang/OTP 17 [erts-6.0] [source-07b8f44] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V6.0  (abort with ^G)
1> prop_delete: Starting eqc mini version 1.0.1 (compiled at {{2010,6,13},{11,15,30}})
....................................................................................................
OK, passed 100 tests
```


## Resources
There is not so much material on the web, so that I try to list below some useful resources:
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
- [Video: Midwest.io 2014 - Property-Based Testing for Better Code by Jessica Kerr](https://www.youtube.com/watch?v=shngiiBfD80)
- [Property-Based Testing and Verification: a Catalog of Classroom Examples](http://www.cs.ou.edu/~rlpage/SEcollab/rlpIFL2011.pdf)
