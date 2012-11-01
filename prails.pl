z(3).
z(4).

row(Row) :-
   b(D),
   z(X),
   content(D, X),
   Row=[
	   'Hello, ',
	   D,
	   '<br>'].

% row ::
%  hbox @ (
%   string('Hello, '),
%   b @ (
%     content: z),
%   string('<br'>)).

rows(Rows) :-
  bagof(R, row(R), Rows).

% rows ::
%   bagof(R, row(R)).

  
show2(X, S) :-
  rows(Rows),
      render(
     Rows, S),
	show(S).

% HTML generation
tag(Name, tag(T)) :-
  cmember(name(Name), T).
  
b(B) :- tag(b, B).

content(tag(A),Text) :-
   cmember(content(Text), A).

text(A, A).
hbox(Out) :- cmember(hbox, Out).



to_simple_array([], []).
to_simple_array([A | B], Z) :-
  ground(A), !, Z=[A | C], to_simple_array(B, C).
to_simple_array([A|_], []).

cmember(A, B) :- member(A, B), !.
	
show(S) :- 
  telling(OldOut),
  tell('out.html'),
  write(S),
  told,
  tell(OldOut).
  

render([H | F], S) :-
  nonvar(H),
  !,
  render(F, S2),
  render(H, HR),
  concat(HR, S2, S).
  
render([], S) :- !, S = ''.


render([A|_], S) :- S = ''.
  

 vmember(A, [A]) :- ground(A), !.
 vmember(A, [A|_]) :- ground(A), !.
 vmember(A, [B|C]) :-
   ground(B), vmember(A, C).
 

 
render(tag(D), S) :- !,
   vmember(name(Name), D),
   vmember(content(Text), D),
   render(['<', Name, '>', Text,
     '</', Name, '>'], S).
 
 
render(H, HR) :-
   atomic(H), !, H = HR.
 
 % Transformation from object oriented hierarchical language to predicates
 
 appendarg(S, Arg, L) :-
   S =.. SL,
   append(SL, [Arg], LL),
   L =.. LL.

:- op(600, xfy, ::).
:- op(600, xfy, .).
:- op(600, xfy, @).
   
 ::(A,B) :-
   transform(A::B, T),
   assert(T).
 
 list_to_comma([A], B) :- !, A=B.
 list_to_comma([A|B], (A,L)) :-
   list_to_comma(B, L).
 
 comma_to_list((A, B), L) :- !, [A|BL], comma_to_list(B, BL).
 comma_to_list(A, [A]).
 
 appendcomma(A, '$empty$', A) :- !.
 appendcomma((A,B), C, D) :- !,
   appendcomma(B, C, D0), D = (A, D0).
 appendcomma(A, B, C) :- C = (A, B).
 
 extractattributes([], Out, '$empty$') :- !.
 extractattributes([A:B|L], Out, E) :- !,
   Clause =.. [B, Z],
   extractattributes(L, Out, LE),
   appendcomma((attribute(Out, A, Z),
     Clause), LE, E).
	 
 extractattributes([A|L], Out, E) :-
   transformelem(A, AN, Clauses),
   container(Out, AN, AEC),
   AE = (AEC, Clauses),
   extractattributes(L, Out, LE),
   appendcomma(AE, LE, E).
 
 transformcall(A.B, O) :- !,
   B =.. [BH | BT],
   O =.. [BH, A | BT].
 
 transformcall(A, A).
   
 
 transformelem(@(E,AL), Out, T) :- !,
   transformcall(E, EC),
   appendarg(EC, Out, ECO),
   extractattributes(AL, Out, Ex),
   T = (ECO, Ex).
 
 transformelem(E, Out, T) :- !,
   transformcall(E, EC),
   appendarg(EC, Out, T).
 
 transform(B::C, BO:-CO) :- 
   appendarg(B, Out, BO),
   transformelem(C, Out, CO).

   container(Big, Small, Out) :- cmember(container(Big, Small), Out).

% Testing helper   
 check(A) :-
   call(A) ,!.
   
 check(A) :-  throw(hello).
 
 
% Basic rendering test  
:- check(render([], '')).
:- check(render('Hello, world', 'Hello, world')).

:- hw::
  text('Hello, world2').
  
:- hw(HW), check(render(HW, 'Hello, world2')).


:- hw2::
  hbox @ [text('A'), text('B')].
:- hw2(HW2), check(render(HW2, 'AB')).

:- demotag::
  b @ [text('A')].
:- demotag(T), check(render(T, '<b>A</b>')).
	 
%%%% Demo for database access
%%%% Needs database with user: id, name; address: id, user_id, address

execute('select user.name as name, address.address as address from user, address where user.id = address.user_id',
        Result) :- Result = ['John', 'Budapest']; Result = ['Jane', 'New York'].

database(user, [id, name]).
database(address, [id, user_id, address]).
user_address(Name, Address) :- user(ID, Name), address(_, ID, Address).

:- user_address_ui::
  hbox @ [
    text('Name: '),
    text(Name),
    text(', address: '),
    text(Address)	]
  :: user_address(Name, Address).
 

:- dbdemo::
  vbox @ [
    user_address_ui ].
 
 check(dbdemo(D), render(D, 'Name: John, address: Budapest<br>Name: Jane, address: New York<br>')).
 

 
% Option 1: O=[name(hbox), contains([name: text, content: asdf]), background_color: B], blue(B).
% Option 2: O=hbox([text([content: asdf]), background_color:B]), blue(B)

% Basic transform test 
 :- check(transform(main::b, main(A):-b(A))).
 :- check(transform(
   main::header @ [background_color:blue],
   main(Out) :- (
     header(Out),
     attribute(Out, background_color, C),
	 blue(C)))).

% Complex transform test
:- check(transform( 
main::	
  vbox @ [
    (header @ [background_color:blue]),
	hbox @ [
	  vbox @ [
	    me.small,
		all_pages]],
      current_page],

  main(V) :- (
    vbox(V),
	container(V, Header),
	header(Header),
	attribute(Header, background_color, B),
	blue(B),
	container(V, HBox),
	hbox(HBox),
	container(HBox, V2),
	vbox(V2),
	container(V2, MS),
  	small(me, MS),
	container(V2, AllPages),
	all_pages(AllPages),
	container(HBox, CurrentPage), 
	current_page(CurrentPage)
	  ))).
 

% Test Option 2
blue('#0000ff').

:- hb::
  hbox @ [
    text(asdf),
	background_color: blue
  ].

:- check(hb(hbox([text([content: asdf]), background_color: '#0000ff']))).


 
 