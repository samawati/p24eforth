only forth definitions hex

wordlist constant target.24
wordlist constant meta.24
wordlist constant asm.24

: (order) ( w wid*n n -- wid*n w n )
  dup if
   1- swap >r recurse over r@ xor if
   1+ r> -rot exit then r> drop then ;
: -order ( wid -- ) get-order (order) nip set-order ;
: +order ( wid -- ) dup >r -order get-order r> swap 1+ set-order ;
: ]asm ( -- ) asm.24 +order ; immediate

get-current meta.24 set-current

: [a] ( "name" -- )
  parse-word asm.24 search-wordlist 0=
   abort" [a]?" compile, ; immediate
: a: ( "name" -- )
  get-current >r asm.24 set-current
   : r> set-current ;

meta.24 +order

a: asm[ ( -- ) asm.24 -order ; immediate

create mask fc0000 , 3f000 , fc0 , 3f ,
create tflash 4000 here over erase allot
variable hi variable hw variable bi

variable tdp

: there tdp @ ;
: tc! tflash + c! ;
: tc@ tflash + c@ ;
: t@ 2 lshift tflash + @ ;
: t! 2 lshift tflash + ! ;
: t, ( n -- ) there t! 1 tdp +! ;
: tw, hw @ t@ xor ffffff and hw @ t! ;
: ti,
  hi @ 10 and if
   0 hi ! there hw ! 0 t, 
  then
  hi @ mask + @ and tw, 4 hi +! ;
: tc, ( c -- )
  bi @ 0 = if
   1 bi ! there hw ! 0 t, 10000 * tw, exit
  then
   bi @ 1 = if
    2 bi ! 100 * tw, exit
   then
   0 bi ! tw, ;
: talign 10 hi ! ;
: tallot ( n -- ) tdp +! ;
: org tdp ! talign ;

variable tlast
variable tuser

     1 constant =ver
     2 constant =ext
    10 constant =base
     3 constant =cell
400000 constant =comp
800000 constant =imed
3fffff constant =mask

80 constant =us

 0 constant =cold
=us =cold + constant =code

4000 constant =em
=em 100 - constant =tib
=tib =us - constant =up

: inst
  dup 6 lshift dup 6 lshift dup 6 lshift + + +
     get-current >r asm.24 set-current create
   r> set-current , does> @ ti, ;

  $01 inst exit
  $0a inst ldi
  $1e inst .

: .,
  begin
   hi @ 10 and 0=
  while
   [a] .
  repeat 0 bi ! there hw ! ;
: thead
  .,
  tlast @ t, there tlast !
     parse-word dup tc, 0 do count tc, loop drop ., ;
: [t] ( -- ; <string> )
  parse-word target.24 search-wordlist 0=
    abort" [t]?" >body @ ; immediate
: save-target ( <name> -- )
  parse-word w/o create-file throw >r
   tflash 4000 r@ write-file throw r> close-file ;
: hex# ( u -- addr len )  0 <# base @ >r hex $a hold # # # # # # r> base ! #> ;
: save-hex ( <name> -- )
  parse-word w/o create-file throw
   4000 0 do i t@ over >r hex# r> write-file throw loop
  close-file throw ;
: compile-only =comp tlast @ t@ xor tlast @ t! ;
: immediate =imed tlast @ t@ xor tlast @ t! ;
: $lit ( -- ) ., [char] " word count dup tc, 0 ?do count tc, loop drop ., ;
: #, ffffff and t, ;
: lit ( d -- ) [a] ldi #, ;
: jump ( d -- ; <string> )
   get-current >r asm.24 set-current create 
   r> set-current , does> @ ., swap 3ffff and xor #, talign ;

100000 jump call
000000 jump jmp
080000 jump bz
0c0000 jump bnc
080000 jump until
0c0000 jump -until

: t: ( -- ; <string> )
  >in @ thead >in !
   ., there create , asm.24 +order does> @ [a] call ;
: t;
  hw @ t@ $fc0000 and 100000 = if
    hw @ dup t@ 100000 xor swap t!
  else
   [a] exit
  then asm.24 -order ;
: u: ( -- ; <string> )
  >in @ thead >in !
    create ., tuser @ dup ,  lit [a] exit 1 tuser +! does> @ lit ;
: [u]
  parse-word target.24 search-wordlist 0=
    abort" [u]?" >body @ =up - 1+ ; immediate
	
    0 tlast !
  =up tuser !
 
]asm

a: begin ., there ;
a: if begin 0 bz ;
a: -if begin 0 bnc ;
a: skip begin 0 jmp ;
a: then dup >r >r begin 3ffff and r> t@ xor r> t! ;
a: else skip swap then ;
a: while if swap ;
a: -while -if swap ;
a: repeat jmp then ;
a: again jmp ;
a: aft ( a -- a' a" ) drop skip begin swap ;

$09 inst @+
$0b inst ld
$0d inst !+
$0f inst st
$10 inst not
$11 inst 2*
$12 inst 2/
$13 inst +*
$14 inst xor
$15 inst and
$16 inst +/
$17 inst +
$18 inst pop
$19 inst a
$1a inst dup
$1c inst push
$1d inst a!
$1f inst drop

a: execute ( a ) push exit ;
a: ! ( n a -- ) a! st ;
a: @ ( a - n ) a! ld ;
a: r> ( - n ) pop ;
a: r@ ( - n ) pop dup push ;
a: >r ( n ) push ;
a: swap ( n1 n2 - n2 n1 ) push a! pop a ;
a: over ( n1 n2 - n1 n2 n1 ) push dup a! pop a ;
a: 2drop ( w w -- ) drop drop ;
a: negate ( n -- -n ) not 1 lit + ;
a: 1- ( a -- a ) -1 lit + ;
a: 1+ ( a -- a ) 1 lit + ;
a: bl ( -- 32 ) 20 lit ;
a: +! ( n a -- ) a! ld + st ;
a: - ( w w -- w ) not + 1 lit + ;
a: for ( -- a ) push begin ;
a: next ( a -- ) r@ while pop 1- push repeat pop drop ;

asm[

target.24 +order target.24 set-current

=cold org

0 #,

there constant =uzero
    0 #, ( hld )
    0 #, ( span )
    0 #, ( >in )
    0 #, ( #tib  )
 =tib #, ( tib )
=base #, ( base)
    0 #, ( context )
    0 #, ( dp ) 
    0 #, ( last ) 
    0 #, ( 'eval )
    0 #, ( 'abort )
    0 #, 0 #, 0 #, 0 #, 0 #, ( tmp )
    0 #, ( text )
	0 #, ( 'boot )
there constant =ulast
=ulast =uzero - constant =udiff

=code org

u: hld
u: span
u: >in
u: #tib
u: tib
u: base =base #,
u: context
u: dp
u: last
u: 'eval
u: 'abort
u: tmp
   4 tuser +!
u: text
u: 'boot

t: dovar pop t;
t: dolit pop a! @+ a push t;
t: donext
    pop pop dup if
	 -1 lit + push push exit
    then
    drop 1 lit +
    push t;
t: 0< ( n - f )
    2*
    -if drop -1 lit exit
    then
    dup xor t;
t: or ( n n - n )
    not push not
    pop and not t;
t: um+ ( n n - n carry )
    +
    -if 1 lit exit
    then
    dup dup xor ( 0 ) t;
t: ?dup ( w -- w w | 0 )
    dup if
	 dup exit 
	then t;
t: rot ( w1 w2 w3 -- w2 w3 w1 )
    push push a! pop pop a t;
t: 2dup ( w1 w2 -- w1 w2 w1 w2 )
    dup push push
    dup a! pop a pop t;
t: dnegate ( d -- -d )
    not push not 1 lit
    + -if
	 pop exit
    then pop 1 lit + t;
t: abs ( n -- +n )
    dup 2* -if 
	 drop not 1 lit + exit
    then drop t;
t: = ( w w -- t )
    xor if 
	 dup dup xor exit 
	then -1 lit t;
t: 2! ( d a -- ) a! push !+ pop st t;
t: 2@ ( a -- d ) a! @+ ld t;
t: count ( b -- b +n ) a! @+ push a pop t;
t: b> ( b a -- b+1 a )
    push a! @+ push
    a pop pop a!
    ld
    2* 2* 2* 2*
    2* 2* 2* 2*
    + st a t;
t: >b ( a b -- a+1 b+3 count )
    push a! @+ push
    a pop pop a!
    dup push 
    $ff lit and pop
    $ffff00 lit and $ff lit xor
    2/ 2/ 2/ 2/
    2/ 2/ 2/ 2/
    dup push
    $ff lit and pop
    $ffff00 lit and $ff lit xor
    2/ 2/ 2/ 2/
    2/ 2/ 2/ 2/
    $ff lit and dup push
    !+ !+ !+
    a pop t;
t: 50us
    2 lit skip
t: 100us
    1 lit
    then
    a! $-89 lit
    begin a +
    -until
    drop t;
t: emit ( c -- )
    $ff lit and
    2* $fffe00 lit xor
    $0b lit
    for 2/ 100us next
    drop t; 
t: key ( -- c )
    $ffffff lit
    begin 2/
    -while ( wait for start bit )
    repeat
    50us
    7 lit
    for
    100us 2/
    -if else $80 lit xor then
    next
    $ff lit and
    100us t; 
t: um* ( u u -- ud )
    a! 0 lit
    +* +* +* +*
    +* +* +* +*
    +* +* +* +*
    +* +* +* +*
    +* +* +* +*
    +* +* +* +*
    push drop a pop t; 
t: um/mod ( ud u -- ur uq )
    not 1 lit + a!
    push a push a!
    pop pop
    skip
t: /mod ( n n -- r q )
    not 1 lit + push
    a! pop 0 lit
    then
    +/ +/ +/ +/
    +/ +/ +/ +/
    +/ +/ +/ +/
    +/ +/ +/ +/
    +/ +/ +/ +/
    +/ +/ +/ +/
    +/ 1 lit xor 2/
    push drop pop a t;
t: mod ( n n -- r ) /mod drop t;
t: / ( n n -- q ) /mod push drop pop t;
t: pack$ ( b u a -- a )
    dup push
    1 lit tmp a! st
    a! dup push st
    a pop
    for aft ( b a )
    b>
    tmp a! ld
    if ld 1 lit xor
    if dup dup xor st
    1 lit +
    else 2 lit st
    then
    else 1 lit st
    then
    then next
    tmp a! ld
    if ld 2 lit xor
    if a! ld
    2* 2* 2* 2*
    2* 2* 2* 2*
    st a
    then
    a! ld
    2* 2* 2* 2*
    2* 2* 2* 2*
    st a
    then
    drop drop pop t;
t: exit pop drop t;
t: execute push t;
t: @ a! ld t;
t: ! a! st t;
t: r> pop a! pop a push t;
t: r@ pop a! pop dup push a push t;
t: >r a! pop push a t;
t: swap push a! pop a t;
t: over push dup a! pop a t;
t: 2drop drop drop t;
t: + + t;
t: not not t;
t: negate not 1 lit + t;
t: 1- -1 lit + t;
t: 1+ 1 lit + t;
t: bl 20 lit t;
t: +! a! ld + st t;
t: - not + 1 lit + t;
t: dup dup t;
t: drop drop t;
t: and and t;
t: xor xor t;
t: u< ( u u -- t ) 2dup xor 0< if swap drop 0< exit then - 0< t;
t: < ( n n -- t ) 2dup xor 0< if drop 0< exit then - 0< t;
t: > ( n n -- t ) swap < t;
t: max ( n n -- n ) 2dup < if swap then drop t;
t: min ( n n -- n ) 2dup swap < if swap then drop t;
t: within ( u ul uh -- t ) over - >r - r> u< t;
t: m/mod ( d n -- r q )
   dup 0< dup >r if
    negate >r dnegate r>
   then >r dup 0< if r@ + then r> um/mod r> if
    swap negate swap
   then t;
t: * ( n n -- n ) um* drop t;
t: m* ( n n -- d )
   2dup xor 0< >r abs swap abs um* r> if
    dnegate then t;
t: */mod ( n n n -- r q ) >r m* r> m/mod t;
t: */ ( n n n -- q ) */mod swap drop t;
t: >char ( c -- c )
   $7f lit and dup $7f lit bl within if
    drop ( char _ ) $5f lit
   then t;
t: here ( -- a ) dp @ t;
t: pad ( -- a ) dp @ 50 lit + t;
t: @execute ( a -- ) @ ?dup if execute then t;
t: cmove ( b b u -- ) for aft >r dup @ r@ ! 1+ r> 1+ then next 2drop t;
t: fill ( b u c -- ) swap for swap aft 2dup ! 1+ then next 2drop t;
t: unpack$ ( a b -- b )
   dup >r ( save b )
   >b $1f lit and 3 lit /
    for aft
     >b drop
    then next 2drop r> t;
t: digit ( u -- c ) 9 lit over < 7 lit and + 30 lit + t;
t: extract ( n base -- n c ) 0 lit swap um/mod swap digit t;
t: <# ( -- ) pad hld ! t;
t: hold ( c -- ) hld @ 1- dup hld ! ! t;
t: # ( u -- u ) base @ extract hold t;
t: #s ( u -- 0 ) begin # dup while repeat t;
t: sign ( n -- ) 0< if ( char - ) 2d lit hold then t;
t: #> ( w -- b u ) drop hld @ pad over - t;
t: str ( n -- b u ) dup >r abs <# #s r> sign #> t;
t: hex ( -- ) 10 lit base ! t;
t: decimal ( -- ) 0a lit base ! t;
t: digit? ( c base -- u t )
   >r 30 lit - 9 lit over < if
    dup 20 lit > if
	 20 lit -
	then
   7 lit - dup 0a lit < or then dup r> u< t;
t: number? ( a -- n t | a f )
   base @ >r 0 lit over count
   over @ 24 lit = if
    hex swap 1+ swap 1-
   then ( a 0 b' n') over @ 2d lit = >r
   swap r@ - swap r@ + ?dup if
    1- for dup >r @ base @ digit?
   while swap base @ * + r> 1+
    next drop r@ if
	  negate then swap
   else r> r> 2drop 2drop 0 lit
   then dup then r> 2drop r> base ! t;
t: space ( -- ) bl emit t;
t: chars ( +n c -- ) swap 0 lit max for aft dup emit then next drop t;
t: spaces ( +n -- ) bl chars t;
t: type ( b u -- )  for aft dup @ >char emit 1+ then next drop t;
t: cr ( -- ) ( =cr ) 0a lit 0d lit emit emit t;
t: do$ ( -- a )
   r> r@ text unpack$
   r@ r> @ =mask lit and $30000 lit / 1+ +
   >r swap >r t;
t: $"| ( -- a ) do$ t;
t: ."| ( -- ) do$ count type t;
t: .r ( n +n -- ) >r str r> over - spaces type t;
t: u.r ( u +n -- ) >r <# #s #> r> over - spaces type t;
t: u. ( u -- ) <# #s #> space type t;
t: . ( n -- )
   base @ 0a lit xor if
    u. exit then str space type t;
t: ? ( a -- ) @ . t;
t: (parse) ( b u c -- b u delta ; <string> )
   tmp ! over >r dup if
    1- tmp @ bl = if
     for bl over @ - 0< not
     while 1+
     next ( b) r> drop 0 lit dup exit
    then r>
   then over swap
   for tmp @ over @ - tmp @ bl = if
      0< then while 1+ next dup >r
     else r> drop dup 1+ >r
      then over - r> r> - exit
     then ( b u) over r> - t;
t: parse ( c -- b u ; <string> ) >r tib @ >in @ + #tib @ >in @ - r> (parse) >in +! t;
t: token ( -- a t; <string> ) bl parse 1f lit min 2dup dup text ! text 1+ swap cmove here 1+ pack$ t;
t: word ( c -- a ; <string> ) parse here 1+ pack$ t;
t: name> ( a -- xt ) dup @ =mask lit and $30000 lit / + 1+ t;
t: same? ( a a u -- a a f \ -0+ )
   $30000 lit /
   for aft over r@ + @
   over r@ + @ - ?dup if
    r> drop exit then
   then next 0 lit t;
t: find ( a va -- xt na | a f )
   swap
   dup @ tmp !
   dup @ >r
   1+ swap
   begin @ dup if
    dup @ =mask lit and r@ xor if
	 1+ -1 lit
    else 1+ tmp @ same?
    then else r> drop swap 1- swap exit then
    while
	  1- 1-
    repeat r> drop swap drop 1- dup name> swap t;
t: name? ( a -- xt na | a f ) context find t;
t: ^tdp ( b b b -- b b b )
    >r over r> swap over xor if
	 8 lit emit
    1- bl emit 8 lit emit then t;
t: tap ( bot eot cur c -- bot eot cur ) dup emit over ! 1+ t;
t: ktap ( bot eot cur c -- bot eot cur )
   dup ( =cr ) 0d lit xor if
    8 lit xor if
	 bl tap
	else ^tdp then exit
    then drop swap drop dup t;
t: accept ( b u -- b u )
   over + over
   begin
     2dup xor
   while
     key dup bl - 5f lit u< if
	  tap
	 else ktap then
   repeat drop over - t;
t: expect ( b u -- ) accept span ! drop t;
t: query ( -- ) tib @ 50 lit accept #tib ! drop 0 lit >in ! t;
t: abort ( -- ) drop 'abort @execute t;
t: <abort"> ( f -- ) if do$ count type abort then do$ drop t; compile-only
t: error ( a -- ) space text count type $3f lit emit cr abort
t: $interpret ( a -- )
   name? ?dup if
    @ =comp lit and
   <abort"> $lit compile-only" execute exit
   then drop text number? if 
    exit 
  then error t;
t: [ ( -- ) [t] $interpret lit 'eval ! t; immediate
t: .ok ( -- )
   [t] $interpret lit 'eval @ = if
    space ."| $lit ok"
   then cr t;
t: eval ( -- )
   begin
    token dup @
   while
    'eval @execute
   repeat drop .ok t;
t: preset =tib lit tib ! t;
t: quit ( -- )
   [ begin
      query eval
	 again t;
t: ' ( -- xt ) token name? if exit then error t;
t: allot ( n -- ) dp +! t;
t: , ( w -- ) here dup 1+ dp ! ! t;
t: [compile] ( -- ; <string> ) ' $100000 lit or , t; immediate
t: compile ( -- ) r> dup @ , 1+ >r t;
t: literal $29e79e lit , , t; immediate
t: $," ( -- ) ( char " ) 22 lit word @ 1+ allot t;
t: ?unique ( a -- a )
   dup name? if
    text count type ."| $lit redef "
   then drop t;
t: $,n ( a -- )
   dup @ if
    ?unique dup
	dup name> dp !
    dup last !
	1-
    context @ swap ! exit then error t;
t: $compile ( a -- )
   name? ?dup if
    @ =imed lit and if
	 execute
    else
	 $3ffff lit and $100000 lit or ,
	then exit
   then drop text number? if
     literal exit
    then error t;
t: overt ( -- ) last @ context ! t;
t: ; ( -- ) $5e79e lit , [ overt t; immediate
t: ] ( -- ) [t] $compile lit 'eval ! t;
t: : ( -- ; <string> ) token $,n ] t;
t: dm+ ( b u -- b )
   over 7 lit u.r space
   for aft dup @ 7 lit u.r 1+ then next t;
t: dump ( b u -- )
   base @ >r hex 8 lit /
   for aft cr 8 lit 2dup dm+
   then next drop r> base ! t;
t: >name ( xt -- na | f )
   context
   begin
    @ dup
   while 2dup name> xor if
    1-
   else swap drop exit
   then repeat swap drop t;
t: .id ( a -- )
    ?dup if
	 text unpack$ count $01f lit and type exit 
	then space ."| $lit {noname}" t;
t: see ( -- ; <string> )
   ' cr
   begin
   20 lit for
   dup @ dup fc0000 lit and
   dup
   if 100000 lit xor then
   if u. space
   else 3ffff lit and >name
   ?dup if .id then
   then 1+
   next key 0d lit =
   until drop t;
t: words ( -- )
   cr context
   begin
    @ ?dup
   while
    dup space .id 1-
   repeat t;
t: .s
    pad a! !+
    !+ !+ !+ !+
    !+ !+ !+ !+
    !+ !+ !+ !+
    !+ !+ !+ !+
    drop pad $10 lit
    for dup ? 1+ next
    drop pad @ cr t;
t: ver ( -- u ) =ver lit 100 lit * =ext lit + t;
t: hi ( -- )
   base @ hex
   cr ."| $lit p24 eforth v"
   ver <# # # 2e lit hold # #>
   type base ! cr t;
t: cold ( -- )
   =uzero lit =up lit =udiff lit cmove 
   preset 
   'boot @execute
   quit t;
t: if ( -- a ) here $80000 lit , t; immediate
t: for ( -- a ) $71e79e lit , here t; immediate
t: begin ( -- a ) here t; immediate
t: ahead ( -- a ) here 0 lit , t; immediate
t: again ( a -- ) , t; immediate
t: then ( a -- ) here swap +! t; immediate
t: next ( a -- ) compile donext , t; immediate
t: until ( a -- ) $80000 lit + , t; immediate
t: repeat ( a a -- ) again [t] then t, t; immediate
t: aft ( a -- a a ) drop ahead begin swap t; immediate
t: else ( a -- a ) ahead swap then t; immediate
t: while ( a -- a a ) if swap t; immediate
t: abort" ( -- ; <string> ) compile <abort"> $," t; immediate
t: $" ( -- ; <string> ) compile $"| $," t; immediate
t: ." ( -- ; <string> ) compile ."| $," t; immediate
t: create ( -- ; <string> ) token $,n overt compile dovar exit
t: variable ( -- ; <string> ) create 0 lit , t;
t: .( ( -- ) 29 lit parse type t; immediate
t: \ ( -- ) #tib @ >in ! t; immediate
t: ( 29 lit parse 2drop t; immediate
t: immediate =imed lit last @ @ or last @ ! t;

target.24 -order set-current

tlast @        [u] context t!
there          [u] dp t!
tlast @        [u] last t!
180000         [t] key t!
1c0000         [t] emit t!
[t] hi         [u] 'boot t!
[t] $interpret [u] 'eval t!
[t] preset     [u] 'abort t!
[t] cold       0 t!

save-target p24.bin
save-hex p24.hex

meta.24 -order

bye
