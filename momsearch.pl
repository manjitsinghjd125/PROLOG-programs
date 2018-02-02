mom:-
	initialfact,
	have(),
	turned_on(flashlight),
	write('Where is MOM ?- Adventure Game'),nl,
	nl,
	write('Mom search is designed by Manjit singh.'),nl,
	nl,
	write('Your sukhman as the adventurer is that of a three year'),nl,
	write('old baby. To win the game find out mom .it is getting'),nl,
	write('late and you''re tired, but you can''t go to sleep'),nl,
	write('without your Mom. Your mission is to find the Mom.'),nl,
	nl,
	write('you control the game using simple English commands'),nl,
	write('expression the action you wish to take.you can go to'),nl,
	write('other rooms,look at your surroundings, look in things'),nl,
	write('take things,drop things,eat things,inventory the'),nl,
	write('things you have , and turn things on and off.'),nl,
	nl,
	write('Type ''help'' if you need more help on mechanics.'),nl,
	write('Type''hint'' if you want a big hint.'),nl,
	write('Type ''quit'' if you give up.'),nl,
	nl,
	write('Enjoy the hunt.'),nl,
	look,
	command_loop.
have().
turned_on(flashlight).

command_loop:-
	repeat,
	get_command(X),
	do(X),
	(momfound;X == quit).
do(goto(X)) :- goto(X),!.
do(help):- mshelp,!.
do(hint):- hint,!.
do(inventory):-inventory,!.
do(take(X)):-take(X),!.
do(drop(X)):- drop(X),!.
do(eat(X)):- eat(X),!.
do(look):- look,!.
do(turn_on(X)):-turn_on(X),!.
do(turn_off(X)):-turn_off(X),!.
do(look_in(X)):-look_in(X),!.
do(quit):-leave,!.

momfound:-
	have(mom),
	write('congratulation , you saved the Mom.'),nl,
	write('Now you can rest secure.'),nl,nl.
leave:-
	write('giving up? it''s going to be a scary night'),nl,
	write('and when you get mom it''s not going'),nl,
	write('to smell right.'),nl,nl.
mshelp:-
	write('Use simple English sentences to enter commands.'),nl,
	write('The commands can cause you to:'),nl,
	nl,
	write('  go to room (ex. go to the office)'),nl,
	write('  look around (ex. look)'),nl,
	write('  look in something (ex. look in the desk)'),nl,
	write('  take something (ex. take the apple)'),nl,
	write('  drop something (ex. drop the apple)'),nl,
	write('  eat something (ex. eat the apple)'),nl,
	write('  turn something on (ex. turn on the light)'),nl,
	write('  inventory your things (ex. inventory)'),nl,
	nl,
	write('Then examples are verbose, terser commands and synonyms'),nl,
	write('are usually accepted.'),nl,nl,
	look.
hint:-
	write('You need to get to store , and you can''t unless'),nl,
	write('you get some light. you can''t turn on the store'),nl,
	write('light, but there is a flashlight on bed in the'),nl,
	write('bedroom you might use.'),nl,nl,
	look.
room(kitchen).
room(office).
room('ground outer').
room(hall).
room('dining room').
room(porch).
room(store).
room(bedroom).
room(bathroom).
room(outer).
room(stair).
room(momdadroom).
room('top front outer').
room('top back outer').
room('top front').

door(bedroom,outer).
door(bedroom,hall).
door(hall,office).
door(hall,kitchen).
door(hall,bathroom).
door(kitchen,'dining room').
door(office,'ground outer').
door('dining room',porch).
door('dining room',stair).
door('dining room',store).
door('top front',momdadroom).
door('top front','top front outer').
door('top front','top back outer').
door('top front',stair).

connect(X,Y):-
	door(X,Y).
connect(X,Y):-
	door(Y,X).
initialfact:-
assertz(location(desk,office)),
assertz(location(table,kitchen)),
assertz(location(apple,table)),
assertz(location(bed,bedroom)),
assertz(location(flashlight,bed)),
assertz(location(water,table)),
assertz(location(computer,office)),
assertz(location(broccoli,table)),
assertz(location(crackers,table)),
assertz(location('washing machine','ground outer')),
assertz(location(mom,bathroom)),
assertz(location('sweet bed',momdadroom)),
assertz(location(daddy,'sweet bed')),
assertz(location(envelope,desk)),
assertz(location(stamp,envelope)),
assertz(location(key,envelope)),
assertz(location(candle,table)),
	assertz(turned_off(flashlight)).
:-dynamic here/1.
:-dynamic have/1.
here(bedroom).



furniture(desk).
furniture('washing machine').
furniture(table).

edible(apple).
edible(crackers).

tastes_yuchy(broccoli).
%goto moves the player
goto(Room):-
	can_go(Room),
	puzzle(goto(Room)),
	moveto(Room),
	look.
goto(_):-
	look.

can_go(Room):-
	here(Here),
	connect(Here,Room),!.
can_go(Room):-
	respond(['You can''t get to ',Room,' from here']),fail.

moveto(Room):-
	retract(here(_)),
	asserta(here(Room)).
%look lists the things in a room
look:-
	here(Here),
	respond(['You are in the ',Here]),
	write('You can see the following things : '),nl,
	list_things(Here),
	write('You can go to the following rooms : '),nl,
	list_connection(Here).
list_things(Place):-
	location(X,Place),
	tab(2),write(X),nl,
	fail.
list_things(_).
list_connection(Place):-
	connect(Place,X),
	tab(2),write(X),nl,
	fail.
list_connection(_).

%look in things
look_in(Thing):-
	location(_,Thing),
	write('The '),write(Thing),
	write(' contains :'),nl,
	list_things(Thing).
look_in(Thing):-
	respond(['There is nothing in the ',Thing]).
%inventory work

take(Thing):-
	is_here(Thing),
	is_takable(Thing),
	move(Thing,have),
	respond(['You now have the ',Thing]).
is_here(Thing):-
	here(Here),
	contains(Thing,Here),!.
is_here(Thing):-
	respond(['There is no ',Thing]),
	fail.
contains(Thing,Here):-
	location(Thing,Here).
contains(Thing,Here):-
	location(Thing,X),
	contains(X,Here).
is_takable(Thing):-
	furniture(Thing),
	respond(['You can''t pick up a ',Thing]),
	!,fail.
is_takable(_).

move(Thing,have):-
	retract(location(Thing,_)),
	asserta(have(Thing)).
drop(Thing):-
	have(Thing),
	here(Here),
	retract(have(Thing)),
	asserta(location(Thing,Here)).
drop(Thing):-
	respond(['You don''t have the ',Thing]).
%eat thing
eat(Thing):-
	have(Thing),
	eat2(Thing).
eat(Thing):-
	respond(['You don''t have the ',Thing]).
eat2(Thing):-
	edible(Thing),
	retract(have(Thing)),
	respond(['That ',Thing,' was good']).
eat2(Thing):-
	tastes_yuchy(Thing),
	respond(['Three year olds don''t eat ',Thing]).
eat2(Thing):-
	write('You can''t eat a '),write(Thing).
%inventory list you have

inventory:-
	have(X),
	write('You have : '),write(X),nl,
	list_possessions.
inventory:-
	write('You have nothing'),nl.
list_possessions:-
	have(X),
	tab(2),write(X),nl,
	fail.
list_possessions.

turn_on(light):-
	respond(['You can''t reach the switch and there is nothing to stand on']).
turn_on(Thing):-
	have(Thing),
	turn_on2(Thing).
turn_on(Thing):-
	respond(['You don''t have the ',Thing]).
turn_on2(Thing):-
	turned_on(Thing),
	respond([Thing,' is already on']).
turn_on2(Thing):-
	turned_off(Thing),
	retract(turned_off(Thing)),
	asserta(turned_on(Thing)),
	respond([Thing,' turned on']).

turn_on2(Thing):-
	respond(['You can''t turn a ',Thing,' on']).
%turn off thing

turn_off(Thing):-
	respond(['I lied about being able to turn things off ',Thing]).
%puzzle for game
puzzle(goto(store)):-
	have(flashlight),
	turn_on(flashlight),
	!.
puzzle(goto(store)):-
	write('You can''t go to the store because it''s dark in the '),nl,
	write('store, and you are afraid of the dark.'),
	!,fail.
puzzle(_).

respond([]):-
	write('.'),nl,nl.
respond([H|T]):-
	write(H),
	respond(T).
get_command(C):-
	read_list(L),
	comm(X,L,[]),
	C=.. X,!.
get_command(_):-
	respond(['I don''t understand, try again or type help']),fail.

%english command making

comm([Pred]) --> verb(intran,Pred).
comm([goto,Arg]) --> noun(go_place,Arg).
comm([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
%recognize three verbs

verb(go_place,goto) --> go_verb.
verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

go_verb -->[go].
go_verb -->[go,to].
go_verb -->[g].

tran_verb(take) --> [take].
tran_verb(take) --> [pick,up].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put,down].
tran_verb(eat) --> [eat].
tran_verb(mshelp) --> [help].
tran_verb(turn_on) --> [turn,on].
tran_verb(turn_on) --> [switch,on].
tran_verb(turn_off) --> [turn,off].
tran_verb(look_in) --> [look,in].
tran_verb(look_in) --> [look].
tran_verb(look_in) --> [open].

intran_verb(inventory) -->[inventory].
intran_verb(inventory) -->[i].
intran_verb(look) -->[look].
intran_verb(look) -->[look,around].
intran_verb(look) -->[l].
intran_verb(quit) -->[quit].
intran_verb(quit) -->[exit].
intran_verb(quit) -->[end].
intran_verb(quit) -->[bye].
intran_verb(mshelp) -->[help].
intran_verb(hint) -->[hint].

nounphrase(Type,Noun) -->det,noun(Type,Noun).
nounphrase(Type,Noun) -->noun(Type,Noun).

det --> [the].
det --> [a].

noun(go_place,R) -->[R],{room(R)}.
noun(go_place,'dining room') --> [dining,room].
noun(go_place,'top front outer') --> [top,front,outer].
noun(go_place,'top back outer') --> [top,front,outer].
noun(go_place,'ground outer') --> [ground,outer].

noun(thing,T) -->[T], {location(T,_)}.
noun(thing,T) -->[T], {have(T)}.
noun(thing,flashlight) -->[flash,light].
noun(thing,'washing machine') -->[washing,machine].
noun(thing,'dirty clothes') -->[dirty,clothes].
noun(thing,'sweet bed') --> [sweet,bed].
noun(thing,light) --> [X,light],{room(X)}.
noun(thing,flashlight) --> [light],{have(flashlight)}.
noun(thing,light) --> [light].

%read data from user

/*read_list(L):-
	write('> '),
	read_line(CL),
	wordlist(L,CL,[]), !.
read_line(L):-
	get0(C),
	buildlist(C,L).
buildlist(13,[]):-!.
buildlist(C,[C|X]):-
	get0(C2),
	buildlist(C2,X).

wordlist([X|Y]) --> word(X),whitespace,wordlist(Y).
wordlist([X]) --> whitespace,wordlist(X).
wordlist([X]) --> word(X).
wordlist([X]) --> word(X),whitespace.

word(W)--> charlist(X),{name(W,X)}.
charlist([X|Y]) --> chr(X),charlist(Y).
charlist([X]) -->chr(X).
chr(X) --> [X],{X>=48}.
whitespace --> whsp,whitespace.
whitespace --> whsp.
whsp-->[X],{X<48}.*/
read_list(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
  get0(C),
  readword(C, W, C1),
  restsent(C1, Ws), !.

restsent(C,[]) :- lastword(C), !.
restsent(C,[W1|Ws]) :-
  readword(C,W1,C1),
  restsent(C1,Ws).

readword(C,W,C1) :-
  single_char(C),
  !,
  name(W, [C]),
  get0(C1).
readword(C, W, C1) :-
  is_num(C),
  !,
  number_word(C, W, C1, _).
readword(C,W,C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1,Cs,C2),
  name(W, [NewC|Cs]).
readword(C,W,C2) :-
  get0(C1),
  readword(C1,W,C2).

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1, Cs, C2).
restword(C, [], C).


single_char(',').
single_char(';').
single_char(':').
single_char('?').
single_char('!').
single_char('.').

in_word(C, C) :- C >= 97, C =< 122.

in_word('`','`').
in_word('-','-').

number_word(C, W, C1, Pow10) :-
  is_num(C),
  !,
  get0(C2),
  number_word(C2, W1, C1, P10),
  Pow10 is P10 * 10,
  W is integer(((C - '0') * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
  C =< 9,
  C >= 0.


lastword(10).
lastword('.').
lastword('!').












































































