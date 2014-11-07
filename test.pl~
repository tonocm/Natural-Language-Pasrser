clear :- write('\e[H\e[2J').
exit :- halt.
/* Code from the section 5.3 of the textbook,
      Programming in Prolog by Clocksin & Mellish.

      The function read_in(S) takes a raw sentence via
      the standard input, and the sentence's list form
      is assigned to the variable S. For example, with
      the input
         A watermelon contains a divine flavor!
      we will obtain
         S = [a,watermelon,contains,a,divine,flavor,!]
      so that we don't have to bother ourselves with
      converting a sentence into a list whenever
      we try to parse it. Note that all capial letters
      become lowercase. Also, it assumes the input
      sentence ends with either '.', '?', or '!'. */

read_in([W|Ws]) :- get_char(C),
                     readword(C,W,C1),
                                        restsent(W,C1,Ws).

restsent(W, _, []) :- lastword(W),!.
restsent(_, C, [W1|Ws]) :- readword(C,W1,C1),
                     restsent(W1,C1,Ws).

readword(C,C,C1) :- single_character(C),!,get_char(C1).
readword(C,W,C2) :-
  in_word(C, NewC),
  !,
  get_char(C1),
  restword(C1,Cs,C2),
  atom_chars(W,[NewC|Cs]).
readword(_,W,C2):-get_char(C1),readword(C1,W,C2). % skipping blanks.

restword(C,[NewC|Cs],C2) :-
  in_word(C,NewC),
  !,
  get_char(C1),restword(C1,Cs,C2).
restword(C,[],C).

in_word(C,C) :- letter(C,_).
in_word(C,L) :- letter(L,C).
in_word(C,C) :- digit(C).
in_word(C,C) :- special_character(C).

special_character('-').
special_character('''').

single_character(',').          single_character(':').
single_character('.').          single_character('?').
single_character(';').          single_character('!').

letter(a,'A').  letter(n,'N').
letter(b,'B').  letter(o,'O').
letter(c,'C').  letter(p,'P').
letter(d,'D').  letter(q,'Q').
letter(e,'E').  letter(r,'R').
letter(f,'F').  letter(s,'S').
letter(g,'G').  letter(t,'T').
letter(h,'H').  letter(u,'U').
letter(i,'I').  letter(v,'V').
letter(j,'J').  letter(w,'W').
letter(k,'K').  letter(x,'X').
letter(l,'L').  letter(y,'Y').
letter(m,'M').  letter(z,'Z').

digit('0').        digit('5').
digit('1').        digit('6').
digit('2').        digit('7').
digit('3').        digit('8').
digit('4').        digit('9').

lastword('.').
lastword('!').
lastword('?').

noun(apple, s).
noun(apples, p).
noun(boy, s).
noun(boys, p).
noun(girl, s).
noun(girls, p).
noun(government, s).
noun(governments, p).
noun(watermelon, s).
noun(watermelons, p).
noun(person, s).
noun(people, p).
det(a, s).
det(an, s).
det(the, s).
det(the, p).
det(any, s).
det(any, p).
det(all, p).
det(some, s).
det(some, p).
verb(conscript, p).
verb(conscripts, s).
verb(likes, s).
verb(like, p).
verb(run, p).
verb(runs, s).
beverb(is, s).
beverb(are, p).
adj(evil).
adj(big).
rel(that, s).
rel(whom, s).
rel(who, p).
rel(which, p).
rel(whose, s).
rel(when, s).
rel(when, p).
rel(where, s).
rel(where, p).
rel(why, s).
rel(why, p).
end(.).
end(?).
end('!').


%Change back S0 to X
sentence(S0) :-
  %read_in(S0),
  sentence(S0,[]).
  
sentence(S0,S) :-
  Quant = 0,
  noun_phrase(S0,S1, Num,Quant),
  verb_phrase(Num,S1,S2,Quant),
  sentence_finisher(S2,S3),
  S3 = S. %Success Test

noun_phrase([X,Y|Z],S1,Num,Quant) :-
  det(X, Num),
  (X = all -> Quant2 is Quant+1, atom_concat('all(x',Quant2,Print),write(Print)), 
  noun(Y, Num),
  S1 = Z.

noun_phrase([X,Y|Z],S1,Num,Quant) :-
  det(X,Num),
  (X = all ->
   Quant2 is Quant+1, atom_concat('all(x',Quant2,Print),write(Print)
  ), 
  noun(Y, Num),
  relcl(Num,Z,Out,Quant2),
  S1 = Out.

noun_phrase([X|Y],S1,Num,Quant) :-
  noun(X, Num),
  S1 = Y.

verb_phrase(Num,[X|Y],S2,Quant) :-
  verb(X,Num),
  noun_phrase(Y,Out,Num2,Quant),
  S2 = Out.

verb_phrase(Num,[X,Y|Z],S2,Quant) :-
  beverb(X,Num),
  adj(Y),
  S2 = Z.

verb_phrase(Num,[X|Y],S2,Quant) :-
  verb(X,Num),
  S2 = Y.

relcl(Num,[X|Y],Out,Quant) :-
  rel(X, Num),
  noun_phrase(Y,[SOut|OutOut],Num,Quant),
  verb(SOut,Num),
  noun_phrase(OutOut,Out3,_,Quant),
  Out = Out3.

relcl(Num,[X|Y],Out,Quant) :-
  rel(X, Num),
  verb_phrase(Num,Y,SOut,Quant),
  Out = SOut.

relcl(Num,[X|Y],Out,Quant) :-
  rel(X, Num),
  noun_phrase(Y,[SOut|OutOut],Num,Quant),
  verb(SOut,Num),
  Out = OutOut.

sentence_finisher([X|Y],S3) :-
  end(X),
  S3 = Y.

/*
sentence(sentence(NP, VP)) -->
  noun_phrase(NP),
  verb_phrase(VP),
  sentence_finisher.
*/
/*
  X Contains number agreement
  Y Contains whether or not it starts with a vowel sound
  noun(N) Dictates how the parse tree will be sent upward
noun(X,Y,noun(N)) -->
  [N], {is_noun(X,Y,N)}.
*/