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

target.24 +order meta.24 +order

a: asm[ ( -- ) asm.24 -order ; immediate

create tflash 4000 here over erase allot
variable hi variable hw variable bi
variable tdp

' @ alias forth@

variable tlast
0 tlast !

: there tdp @ ;
: t@ 2 lshift tflash + @ ;
: t! 2 lshift tflash + ! ;
: t, ( n -- ) there t! 1 tdp +! ;
: talign 10 hi ! ;
: tallot ( n -- ) tdp +! ;
: org tdp ! talign ;

create mask fc0000 , 3f000 , fc0 , 3f ,

: #, ffffff and t, ;
: ,w hw @ t@ xor ffffff and hw @ t! ;
: ,i hi @ 10 and if 0 hi ! there hw ! 0 #, then
  hi @ mask + @ and ,w 4 hi +! ;
: b, ( c )
  bi @ 0 = if
   1 bi ! there hw ! 0 #, 10000 * ,w exit
  then
   bi @ 1 = if
    2 bi ! 100 * ,w exit
   then
   0 bi ! ,w ;
: inst
  dup 6 lshift dup 6 lshift dup 6 lshift + + +
       get-current >r asm.24 set-current create
   r> set-current , does> @ ,i ;

  $1e inst nop

: nop,
  begin
   hi @ 10 and 0=
  while
   [a] nop
  repeat 0 bi ! there hw ! ;
: thead
  nop,
  tlast @ t, there tlast !
     parse-word dup b, 0 do count b, loop drop nop, ;
: [t] ( -- ; <string> )
  parse-word target.24 search-wordlist 0=
    abort" [t]?" >body @ ; immediate
: save-target ( <name> -- )
  parse-word w/o create-file throw >r
   tflash 8000 r@ write-file throw r> close-file ;
: hex# ( u -- addr len )  0 <# base @ >r hex $a hold # # # # # # r> base ! #> ;
: save-hex ( <name> -- )
  parse-word w/o create-file throw
   4000 0 do i t@ over >r hex# r> write-file throw loop
  close-file throw ;
: compile-only 400000 tlast @ t@ xor tlast @ t! ;
: immediate 800000 tlast @ t@ xor tlast @ t! ;
: $lit ( -- ) nop, [char] " word count dup b, 0 ?do count b, loop drop nop, ;

: jump create , does> @ nop, swap 3ffff and xor #, talign ;
: -;' hw @ t@ dup $fc0000 and 100000 = if
    100000 xor hw @ t!
  else drop then ;
: ldi 28a28a ,i #, ;

100000 jump call
000000 jump jmp
080000 jump bz
0c0000 jump bnc
080000 jump until
0c0000 jump -until

: code
  >in @ thead >in !
   nop, there get-current >r target.24 set-current create
    r> set-current , asm.24 +order does> @ call ;
: t:
  >in @ thead >in !
   nop, there create , does> @ call ;

]asm

: begin nop, there ;
: if begin 0 bz ;
: -if begin 0 bnc ;
: skip begin 0 jmp ;
: then dup >r >r begin 3ffff and r> t@ xor r> t! ;
: else skip swap then ;
: while if swap ;
: -while -if swap ;
: repeat jmp then ;
: again jmp ;

]asm

$01 inst ret
$09 inst ldp
$0b inst ld
$0d inst stp
$0f inst st
$10 inst com
$11 inst shl
$12 inst shr
$13 inst mul
$14 inst xor
$15 inst and
$16 inst div
$17 inst add
$18 inst pop
$19 inst lda
$1a inst dup
$1c inst push
$1d inst sta
$1f inst drop

]asm

a: execute ( a ) push ret ;
a: ! ( n a -- ) sta st ;
a: @ ( a - n ) sta ld ;
a: r> ( - n ) pop ;
a: r@ ( - n ) pop dup push ;
a: >r ( n ) push ;
a: swap ( n1 n2 - n2 n1 ) push sta pop lda ;
a: over ( n1 n2 - n1 n2 n1 ) push dup sta pop lda ;
a: 2drop ( w w -- ) drop drop ;
a: + ( w w -- w ) add ;
a: not ( w -- w ) com ;
a: negate ( n -- -n ) com 1 ldi add ;
a: 1- ( a -- a ) -1 ldi add ;
a: 1+ ( a -- a ) 1 ldi add ;
a: bl ( -- 32 ) 20 ldi ;
a: +! ( n a -- ) sta ld add st ;
a: - ( w w -- w ) com add 1 ldi add ;
a: exit ret ;

asm[

: t; [a] ret ;
: end-code [a] ret asm.24 -order ;

: hld 700 ldi ;
: span 701 ldi ;
: >in 702 ldi ;
: #tib 703 ldi ;
: 'tib 704 ldi ;
: base 705 ldi ;
: context 706 ldi ;
: cp 707 ldi ;
: last 708 ldi ;
: 'eval 709 ldi ;
: 'abort 70a ldi ;
: text 710 ldi ;
: tmp 70b ldi ;

$18 org

code dovar
    pop end-code
code dolit
    pop sta ldp
    lda push end-code
code donext
    pop pop dup if
	 -1 ldi add push
     push ret
    then
    drop 1 ldi add
    push end-code
code 0< ( n - f )
    shl
    -if drop -1 ldi ret
    then
    dup xor end-code
code or ( n n - n )
    com push com
    pop and com end-code
code um+ ( n n - n carry )
    add
    -if 1 ldi ret
    then
    dup dup xor ( 0 ) end-code
code ?dup ( w -- w w | 0 )
    dup
    if dup ret then end-code
code rot ( w1 w2 w3 -- w2 w3 w1 )
    push push sta pop
    pop lda end-code
code 2dup ( w1 w2 -- w1 w2 w1 w2 )
    dup push push
    dup sta pop lda pop end-code
code dnegate ( d -- -d )
    com push com 1 ldi
    add
    -if pop ret
    then
    pop 1 ldi add end-code
code abs ( n -- +n )
    dup shl
    -if drop com 1 ldi add
    ret
    then
    drop end-code
code = ( w w -- t )
    xor
    if dup dup xor ret then
    -1 ldi end-code
code 2! ( d a -- )
    sta push stp
    pop st end-code
code 2@ ( a -- d )
    sta ldp ld end-code
code count ( b -- b +n )
    sta ldp push lda
    pop end-code
code b> ( b a -- b+1 a )
    push sta ldp push
    lda pop pop sta
    ld
    shl shl shl shl
    shl shl shl shl
    add st lda end-code
code >b ( a b -- a+1 b+3 count )
    push sta ldp push
    lda pop pop ( a+1 n b ) sta
    dup push
    $ff ldi and pop
    $ffff00 ldi and $ff ldi xor
    shr shr shr shr
    shr shr shr shr
    dup push
    $ff ldi and pop
    $ffff00 ldi and $ff ldi xor
    shr shr shr shr
    shr shr shr shr
    $ff ldi and dup push
    stp stp stp ( a+1 c )
    lda pop end-code

: for ( -- a ) [a] push begin ;
: next ( a -- ) donext jmp ;
: <next> next ;
: aft ( a -- a' a" ) drop begin 0 jmp begin swap ;
: lit ( d -- ) ldi ;

target.24 +order asm.24 +order

code 50us
    2 ldi skip
code 100us
    1 ldi
    then
    sta $-89 ldi
    begin lda add
    -until
    drop
    end-code
code emit ( c -- )
    $ff ldi and
    shl $fffe00 ldi xor
    $0b ldi
    for shr 100us next
    drop end-code
code key ( -- c )
    $ffffff ldi
    begin shr
    -while ( wait for start bit )
    repeat
    50us
    7 ldi
    for
    100us shr
    -if else $80 ldi xor then
    next
    $ff ldi and
    100us -;'
code um* ( u u -- ud )
    sta 0 ldi
    mul mul mul mul
    mul mul mul mul
    mul mul mul mul
    mul mul mul mul
    mul mul mul mul
    mul mul mul mul
    push drop lda pop end-code
code um/mod ( ud u -- ur uq )
    com 1 ldi add sta
    push lda push sta
    pop pop
    skip
code /mod ( n n -- r q )
    com 1 ldi add push
    sta pop 0 ldi
    then
    div div div div
    div div div div
    div div div div
    div div div div
    div div div div
    div div div div
    div 1 ldi xor shr
    push drop pop lda end-code
code mod ( n n -- r ) /mod drop end-code
code / ( n n -- q ) /mod push drop pop end-code
code pack$ ( b u a -- a )
    dup push
    1 ldi tmp sta st
    sta dup push st
    lda pop
    for aft ( b a )
    b>
    tmp sta ld
    if ld 1 ldi xor
    if dup dup xor st
    1 ldi add
    else 2 ldi st
    then
    else 1 ldi st
    then
    then next
    tmp sta ld
    if ld 2 ldi xor
    if sta ld
    shl shl shl shl
    shl shl shl shl
    st lda
    then
    sta ld
    shl shl shl shl
    shl shl shl shl
    st lda
    then
    drop drop pop end-code
code exit pop drop end-code
code execute push end-code
code ! sta st end-code
code @ sta ld end-code
code r> pop sta pop lda push end-code
code r@ pop sta pop dup push lda push end-code
code >r sta pop push lda end-code
code swap push sta pop lda end-code
code over push dup sta pop lda end-code
code 2drop drop drop end-code
code + add end-code
code not com end-code
code negate com 1 ldi add end-code
code 1- -1 ldi add end-code
code 1+ 1 ldi add end-code
code bl 20 ldi end-code
code +! sta ld add st end-code
code - com add 1 ldi add end-code
code dup dup end-code
code drop drop end-code
code and and end-code
code xor xor end-code
code com com end-code

asm.24 +order target.24 set-current

t: u< ( u u -- t ) 2dup xor 0< if swap drop 0< exit then - 0< -;'
t: < ( n n -- t ) 2dup xor 0< if drop 0< exit then - 0< -;'
t: > ( n n -- t ) swap < t;
t: max ( n n -- n ) 2dup < if swap then drop t;
t: min ( n n -- n ) 2dup swap < if swap then drop t;
t: within ( u ul uh -- t ) over - >r - r> u< -;'
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
t: */mod ( n n n -- r q ) >r m* r> m/mod -;'
t: */ ( n n n -- q ) */mod swap drop t;
t: >char ( c -- c )
   $7f lit and dup $7f lit bl within if
    drop ( char _ ) $5f lit
   then t;
t: here ( -- a ) cp @ t;
t: pad ( -- a ) cp @ 50 lit + t;
t: tib ( -- a ) 'tib @ t;
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
t: extract ( n base -- n c ) 0 lit swap um/mod swap digit -;'
t: <# ( -- ) pad hld ! t;
t: hold ( c -- ) hld @ 1- dup hld ! ! t;
t: # ( u -- u ) base @ extract hold -;'
t: #s ( u -- 0 ) begin # dup while repeat t;
t: sign ( n -- ) 0< if ( char - ) 2d lit hold then t;
t: #> ( w -- b u ) drop hld @ pad over - t;
t: str ( n -- b u ) dup >r abs <# #s r> sign #> -;'
t: hex ( -- ) 10 lit base ! t;
t: decimal ( -- ) 0a lit base ! t;
t: digit? ( c base -- u t )
   >r 30 lit - 9 lit over < if
    dup 20 lit > if
	 20 lit -
	then
   7 lit - dup 0a lit < or then dup r> u< -;'
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
t: space ( -- ) bl emit -;'
t: chars ( +n c -- ) swap 0 lit max for aft dup emit then next drop t;
t: spaces ( +n -- ) bl chars -;'
t: type ( b u -- )  for aft dup @ >char emit 1+ then next drop t;
t: cr ( -- ) ( =cr ) 0a lit 0d lit emit emit -;'
t: do$ ( -- a )
   r> r@ text unpack$
   r@ r> @ $3fffff lit and $30000 lit / 1+ +
   >r swap >r t;
t: $"| ( -- a ) do$ -;'
t: ."| ( -- ) do$ count type -;'
t: .r ( n +n -- ) >r str r> over - spaces type -;'
t: u.r ( u +n -- ) >r <# #s #> r> over - spaces type -;'
t: u. ( u -- ) <# #s #> space type -;'
t: . ( n -- )
   base @ 0a lit xor if
    u. exit then str space type -;'
t: ? ( a -- ) @ . -;'
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
t: parse ( c -- b u ; <string> ) >r tib >in @ + #tib @ >in @ - r> (parse) >in +! t;
t: token ( -- a t; <string> ) bl parse 1f lit min 2dup dup text ! text 1+ swap cmove here 1+ pack$ -;'
t: word ( c -- a ; <string> ) parse here 1+ pack$ -;'
t: name> ( a -- xt ) dup @ $3fffff lit and $30000 lit / + 1+ t;
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
    dup @ $3fffff lit and r@ xor if
	 1+ -1 lit
    else 1+ tmp @ same?
    then else r> drop swap 1- swap exit then
    while
	  1- 1-
    repeat r> drop swap drop 1- dup name> swap t;
t: name? ( a -- xt na | a f ) context find -;'
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
t: query ( -- ) tib 50 lit accept #tib ! drop 0 lit >in ! t;
t: abort ( -- ) 'abort @execute t;
t: <abort"> ( f -- ) if do$ count type abort then do$ drop t;
t: error ( a -- ) space text count type $3f lit emit cr abort
t: $interpret ( a -- )
   name? ?dup if
    @ 400000 lit and
   <abort"> $lit compile only" execute exit
   then drop text number? if exit then error
t: [ ( -- ) [t] $interpret lit 'eval !
t; immediate
t: .ok ( -- )
   [t] $interpret lit 'eval @ =
   if ."| $lit  ok" cr then t;
t: eval ( -- )
   begin
    token dup @
   while
    'eval @execute
   repeat drop .ok -;'
t: quit ( -- ) $730 lit 'tib ! [ begin query eval again
t: ' ( -- xt ) token name? if exit then error
t: allot ( n -- ) cp +! t;
t: , ( w -- ) here dup 1+ cp ! ! t;
t: [compile] ( -- ; <string> ) ' $100000 lit or , -;' immediate
t: compile ( -- ) r> dup @ , 1+ >r t;
t: literal $29e79e lit , , -;' immediate
t: $," ( -- ) ( char " ) 22 lit word @ 1+ allot -;'
t: ?unique ( a -- a )
   dup name? if
    text count type ."| $lit redef "
   then drop t;
t: $,n ( a -- )
   dup @ if
    ?unique dup
	dup name> cp !
    dup last !
	1-
    context @ swap ! exit then error
t: $compile ( a -- )
   name? ?dup if
    @ $800000 lit and if
	 execute
    else $3ffff lit and $100000 lit or , then exit
   then drop text number? if
     literal exit then error
t: overt ( -- ) last @ context ! t;
t: ; ( -- ) $5e79e lit , [ overt -;' immediate
t: ] ( -- ) [t] $compile lit 'eval ! t;
t: : ( -- ; <string> ) token $,n ] -;'
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
t: .id ( a -- ) ?dup if text unpack$ count $01f lit and type exit then space ."| $lit {noname}" -;'
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
code .s
    pad sta stp
    stp stp stp stp
    stp stp stp stp
    stp stp stp stp
    stp stp stp stp
    drop pad $10 lit
    for dup ? 1+ next
    drop pad @ cr -;'
t: cold ( -- )
   cr ."| $lit p24 v"
   66 lit <# # # ( char . ) 2e lit hold # #> type
   cr quit
t: if ( -- a ) here $80000 lit , -;' immediate
t: for ( -- a ) $71e79e lit , here -;' immediate
t: begin ( -- a ) here -;' immediate
t: ahead ( -- a ) here 0 lit , -;' immediate
t: again ( a -- ) , -;' immediate
t: then ( a -- ) here swap +! t; immediate
t: next ( a -- ) compile donext , -;' immediate
t: until ( a -- ) $80000 lit + , -;' immediate
t: repeat ( a a -- ) again then -;' immediate
t: aft ( a -- a a ) drop ahead begin swap t; immediate
t: else ( a -- a ) ahead swap then -;' immediate
t: while ( a -- a a ) if swap t; immediate
t: abort" ( -- ; <string> ) compile <abort"> $," t; immediate
t: $" ( -- ; <string> ) compile $"| $," t; immediate
t: ." ( -- ; <string> ) compile ."| $," t; immediate
t: code ( -- ; <string> ) token $,n overt -;'
t: create ( -- ; <string> ) code dovar t;
t: variable ( -- ; <string> ) create 0 lit , -;'
t: .( ( -- ) 29 lit parse type -;' immediate
t: \ ( -- ) #tib @ >in ! t; immediate
t: ( 29 lit parse 2drop t; immediate
t: immediate $800000 lit last @ @ or last @ ! t;

asm.24 -order target.24 -order set-current

0 org

]asm 10 lit 704 lit 6 lit asm[

[t] cold lit
]asm
   push push
   nop, there
   push sta ldp push
   lda pop pop sta
   stp lda
   <next>
   drop drop ret
asm[

$10 org

730 #,
0a #,
tlast @ #,
780 #,
tlast @ #,

[t] $interpret #,
[t] quit #,

180000 b7 t!
1c0000 aa t!

save-target p24.bin
save-hex p24.hex

meta.24 -order

bye
