%% Following are the operators.
:- op(500,xfy,&).
:- op(510,xfy,=>).
:- op(100,fx,.).

%% This is to run the program. 
%% It will run in loop until a user passes halt in the input.
%% It will parse the statements and give answer when questions are asked.
run:-
	write(-->),
	input(X),
	X \= [halt] -> 
		work_on_input(X, R),
		write(R),
		nl,
		run;
		true.
%% This takes S as input, these are setences to whom we want a reply and gives a reply in R.
work_on_input(S, R) :-
	parse(S, LF, T),
	clause(LF, C, F), !,
	answer(T, F, C, R).
%% If no parse was found then it replies as Sentence too difficult to parse.
work_on_input(_, error("Sentence too difficult to parse")).

%% Following rule gives a reply, if a query is asked.
%% Here F is a variable which are to be interpreted.
%% A is ans, c is the clause being used and R is a reply
answer(query, F, (answer(A):- C), R) :-
	(setof(A, F^C, A)
		-> R = answer(A)
		; (A = []
		-> R = answer([none])
		; R = answer([no]))), !.
%% It we are not asking something, then we want to assert something so it comes here.
%% We assert here and tell user that what we have inserted.
answer(assertion, _, A, asserted(A)) :-
	assert(A), 
	!.
%% If a sentence appears to be very different, then reply with the error.
answer( _ , _ , _ , error("Error is unknown")).

answer(error(ET)) :-
	write(ET), write("."), nl.
answer(asserted(A)) :-
	write("Asserted "), write(A), write("."), nl.
answer(answer(Ans)) :-
	write_answers(Ans).

write_answers([Ans]) :- 
	!,
	write(Ans), write("."), nl.
	write_answers([Ans|Rest]) :-
	write(Ans), write(", "),
	write_answers(Rest).

%% Here the three variables are sentence, logical form of sentence and type,
%% that whether it is query or assertion
parse(Sentence, LF, assertion) :-
	s(LF, nogap, Sentence, []).

parse(Sentence, LF, query) :-
	q(LF, Sentence, []).

%% Using following rule we convert a statements to there clausal form.
clause(all(X,F0),F,[X|V]) :- clause(F0,F,V).
clause(A0=>C0,(C:-A),V) :-
	clause_literal(C0,C),
	clause_antecedent(A0,A,V).
clause(C0,C,[]) :- clause_literal(C0,C).


clause_antecedent(L0,L,[]) :- clause_literal(L0,L).
clause_antecedent(E0&F0,(E,F),V) :-
	clause_antecedent(E0,E,V0),
	clause_antecedent(F0,F,V1),
	conc(V0,V1,V).



clause_antecedent(exists(X,F0),F,[X|V]) :-
	clause_antecedent(F0,F,V).
clause_literal(.L,L).
q(S => .answer(X)) --> whpron, vp(finite, X^S, nogap).
q(S => .answer(X)) --> whpron, sinv(S, gap(np, X)).
q(S => .answer(yes)) --> sinv(S, nogap).
q(S => .answer(yes)) -->
	[is],
	np((X^S0)^S, nogap),
	np((X^true)^exists(X,S0&true), nogap).
s(S, GapInfo) --> 
	np(VP^S, nogap),
	vp(finite, VP, GapInfo).
sinv(S, GapInfo) -->
	aux(finite/Form, VP1^VP2),
	np(VP2^S, nogap),
	vp(Form, VP1, GapInfo).
np(NP, nogap) -->
	det(N2^NP), n(N1), optrel(N1^N2).
np(NP, nogap) --> pn(NP).
np((X^S)^S, gap(np, X)) --> [].





%%% Verb Phrases 
vp(Form, X^S, GapInfo) --> tv(Form, X^VP), np(VP^S, GapInfo).
vp(Form, VP, nogap) --> iv(Form, VP).
vp(Form1, VP2, GapInfo) -->
	aux(Form1/Form2, VP1^VP2),
	vp(Form2, VP1, GapInfo).
vp(Form1, VP2, GapInfo) -->
	rov(Form1/Form2, NP^VP1^VP2),
	np(NP, GapInfo),

vp(Form2, VP1, nogap).
vp(Form2, VP2, GapInfo) -->
	rov(Form1/Form2, NP^VP1^VP2),
	np(NP, nogap),
	vp(Form1, VP1, GapInfo).
vp(finite, X^S, GapInfo) -->
	[is],
	np((X^P)^exists(X,S&P), GapInfo).
%%% Relative Clauses
optrel((X^S1)^(X^(S1&S2))) --> relpron, vp(finite,X^S2, nogap).
optrel((X^S1)^(X^(S1&S2))) --> relpron, s(S2, gap(np, X)).
optrel(N^N) --> [].

det(LF) --> [D], {det(D, LF)}.
n(LF) --> [N], {n(N, LF)}.
pn((E^S)^S) --> [PN], {pn(PN, E)}.
aux(Form, LF) --> [Aux], {aux(Aux, Form, LF)}.
relpron --> [RP], {relpron(RP)}.
whpron --> [WH], {whpron(WH)}.

iv(nonfinite, LF) --> [IV], {iv(IV, _, _, _, _, LF)}.
iv(finite, LF) --> [IV], {iv(_, IV, _, _, _, LF)}.
iv(finite, LF) --> [IV], {iv(_, _, IV, _, _, LF)}.
iv(past_participle, LF) --> [IV], {iv(_, _, _, IV, _, LF)}.
iv(pres_participle, LF) --> [IV], {iv(_, _, _, _, IV, LF)}.
tv(nonfinite, LF) --> [TV], {tv(TV, _, _, _, _, LF)}.
tv(finite, LF) --> [TV], {tv(_, TV, _, _, _, LF)}.
tv(finite, LF) --> [TV], {tv(_, _, TV, _, _, LF)}.
tv(past_participle, LF) --> [TV], {tv(_, _, _, TV, _, LF)}.

tv(pres_participle, LF) --> [TV], {tv(_, _, _, _, TV, LF)}.
rov(nonfinite /Requires, LF)
--> [ROV], {rov(ROV, _, _, _, _, LF, Requires)}.
rov(finite /Requires, LF)
--> [ROV], {rov(_, ROV, _, _, _, LF, Requires)}.
rov(finite /Requires, LF)
--> [ROV], {rov(_, _, ROV, _, _, LF, Requires)}.
rov(past_participle/Requires, LF)
--> [ROV], {rov(_, _, _, ROV, _, LF, Requires)}.
rov(pres_participle/Requires, LF)
--> [ROV], {rov(_, _, _, _, ROV, LF, Requires)}.

relpron( that ).
relpron( who ).
relpron( whom ).
whpron( who ).
whpron( whom ).
whpron( what ).
det( every, (X^S1)^(X^S2)^ all(X,S1=>S2) ).
det( a, (X^S1)^(X^S2)^exists(X,S1&S2) ).
det( some, (X^S1)^(X^S2)^exists(X,S1&S2) ).
n( author, X^ .author(X) ).
n( novel, X^ .novel(X) ).
n( professor, X^ .professor(X) ).
n( program, X^ .program(X) ).
n( programmer, X^ .programmer(X) ).
n( student, X^ .student(X) ).
pn( jack, jack ).
pn( bill, bill ).
pn( gottlob, gottlob ).
pn( john, john ).
pn( 1984, 1984 ).
pn( tom, tom ).
pn( terry, terry ).
iv( halt, halts, halted,
halted, halting, X^ .halt(X) ).
tv( write, writes, wrote, written, writing, X^Y^ .writes(X,Y) ).
tv( meet, meets, met, met, meeting, X^Y^ .meets(X,Y) ).
tv( concern, concerns, concerned, concerned, concerning, X^Y^ .concerns(X,Y) ).
tv( run, runs, ran, run, running, X^Y^ .runs(X,Y) ).
rov( want, wants, wanted, wanted, wanting, ((X^ .want(Y,X,Comp))^S) ^ (X^Comp) ^ Y ^ S,infinitival).
aux( to, infinitival/nonfinite, VP^ VP ).
aux( does, finite/nonfinite, VP^ VP ).
aux( did, finite/nonfinite, VP^ VP ).

conc([], List, List).
conc([Element|Rest], List, [Element|LongRest]) :-
conc(Rest, List, LongRest).

%% Following rule helps us to take input from user and return list of words. 
input(Words) :-
get0(Char), % prime the lookahead
input(Char, Words). % get the words
input(C, []) :- newline(C), !.

input(C, Words) :- space(C), !,
get0(Char),
input(Char, Words).
input(Char, [Word|Words]) :-
word(Char, Chars, Next), % get the word
name(Word, Chars), % pack the characters
input(Next, Words).

word(C, [], C) :- space(C), !.
word(C, [], C) :- newline(C), !.
word(Char, [Char|Chars], Last) :-
get0(Next),
word(Next, Chars, Last).
space(32).
newline(10).
