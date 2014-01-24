#!/bin/sh

ping6 -c 10 aaaa::212:7400:117d:5233 > tests.txt
erl -pa ../erl_coap/ebin -noshell -s test get_multiple aaaa::212:7400:117d:5233 hello -s init stop
ping6 -c 10 aaaa::212:7400:117d:5233 >> tests.txt

