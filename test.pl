#@ Automated test for S-SymObj (make test)
use Test::Simple tests => 17;

BEGIN { require SymObj; $SymObj::Debug = 0; }
my ($o, $v, @va, %ha, $m);


{package X1;
   BEGIN {
      SymObj::sym_create(SymObj::NONE, { # (NONE is 0..)
            _name => '', _array => [qw(av1 av2)],
            _hash => {hk1 => 'hv1', hk2 => 'hv2'} });
   }
}
{package X2;
   our (@ISA); BEGIN { @ISA = ('X1'); SymObj::sym_create(0, {}); }
}

$o = X2->new(name => 'EASY T1');
ok($o->name eq 'EASY T1');

$v = $o->name('EASY T2');
ok($o->name eq 'EASY T2' && $v eq $o->name);

$o->array(   '1_1');  $o->array('2_1');
$o->array(qw( 1_2                2_2));
$o->array([qw(1_3                2_3)]);
$o->array(   '1_4' =>           '2_4');
ok($o->array->[0] eq 'av1' && $o->array->[1] eq 'av2' &&
   $o->array->[2] eq '1_1' && $o->array->[3] eq '2_1' &&
   $o->array->[4] eq '1_2' && $o->array->[5] eq '2_2' &&
   $o->array->[6] eq '1_3' && $o->array->[7] eq '2_3' &&
   $o->array->[8] eq '1_4' && $o->array->[9] eq '2_4');

$v = $o->array;
ok($v->[0] eq 'av1' && $v->[1] eq 'av2' &&
   $v->[2] eq '1_1' && $v->[3] eq '2_1' &&
   $v->[4] eq '1_2' && $v->[5] eq '2_2' &&
   $v->[6] eq '1_3' && $v->[7] eq '2_3' &&
   $v->[8] eq '1_4' && $v->[9] eq '2_4');

@va = $o->array();
ok($va[0] eq 'av1' && $va[1] eq 'av2' &&
   $va[2] eq '1_1' && $va[3] eq '2_1' &&
   $va[4] eq '1_2' && $va[5] eq '2_2' &&
   $va[6] eq '1_3' && $va[7] eq '2_3' &&
   $va[8] eq '1_4' && $va[9] eq '2_4');

$o->hash(    i_1 => 'yo1',  we_1 => 'al1');
$o->hash(   'i_2',  'yo2', 'we_2',  'al2');
$o->hash(qw( i_3     yo3    we_3     al3));
$o->hash([qw(i_4     yo4    we_4     al4)]);
$o->hash({   i_5 => 'yo5',  we_5 => 'al5'});
ok($o->hash->{hk1} eq 'hv1' && $o->hash->{hk2} eq 'hv2' &&
   $o->hash->{i_1} eq 'yo1' && $o->hash->{we_1} eq 'al1' &&
   $o->hash->{i_2} eq 'yo2' && $o->hash->{we_2} eq 'al2' &&
   $o->hash->{i_3} eq 'yo3' && $o->hash->{we_3} eq 'al3' &&
   $o->hash->{i_4} eq 'yo4' && $o->hash->{we_4} eq 'al4' &&
   $o->hash->{i_5} eq 'yo5' && $o->hash->{we_5} eq 'al5');

$v = $o->hash;
ok($v->{hk1} eq 'hv1' && $v->{hk2} eq 'hv2' &&
   $v->{i_1} eq 'yo1' && $v->{we_1} eq 'al1' &&
   $v->{i_2} eq 'yo2' && $v->{we_2} eq 'al2' &&
   $v->{i_3} eq 'yo3' && $v->{we_3} eq 'al3' &&
   $v->{i_4} eq 'yo4' && $v->{we_4} eq 'al4' &&
   $v->{i_5} eq 'yo5' && $v->{we_5} eq 'al5');

%ha = $o->hash();
ok($ha{hk1} eq 'hv1' && $ha{hk2} eq 'hv2' &&
   $ha{i_1} eq 'yo1' && $ha{we_1} eq 'al1' &&
   $ha{i_2} eq 'yo2' && $ha{we_2} eq 'al2' &&
   $ha{i_3} eq 'yo3' && $ha{we_3} eq 'al3' &&
   $ha{i_4} eq 'yo4' && $ha{we_4} eq 'al4' &&
   $ha{i_5} eq 'yo5' && $ha{we_5} eq 'al5');


%{X1::hash()} = ();
X1::hash(newhk1=>'newhv1', newhk2=>'newhv2');
$o = X2->new(name => 'EASY T3');
ok($o->name eq 'EASY T3' && $o->hash->{newhk1} eq 'newhv1' &&
   $o->hash->{newhk2} eq 'newhv2');


{package T1_0;
   BEGIN {
      SymObj::sym_create(0, { _i1 => 'T1_0', _n => 'T1_0', _v => 1 },
         sub { my ($self, $pkg) = @_; ::ok($m == 0); $m |= 0b00000001; });
   }}
{package T1_1;
   our (@ISA); BEGIN { @ISA = (qw(T1_0));
      SymObj::sym_create(0, { _i2 => 'T1_1', _n => 'T1_1', _v => 2 },
         sub { my ($self, $pkg) = @_; ::ok($m == 0b1); $m |= 0b00000010; });
   }}
{package T1_2;
   our (@ISA); BEGIN { @ISA = (qw(T1_1));
      SymObj::sym_create(0, { _i3 => 'T1_2', _n => 'T1_2', _v => 3 },
         sub { my ($self, $pkg) = @_; ::ok($m == 0b11); $m |= 0b00000100; });
   }}

{package T2_0;
   BEGIN {
      SymObj::sym_create(0, { _i4 => 'T2_0', _n => 'T2_0', _v => 4 },
         sub { my ($self, $pkg) = @_; ::ok($m == 0b111); $m |= 0b00001000; });
   }}
{package T2_1;
   our (@ISA); BEGIN { @ISA = (qw(T2_0));
      SymObj::sym_create(0, { _i5 => 'T2_1', _n => 'T2_1', _v => 5 },
         sub { my ($self, $pkg) = @_; ::ok($m == 0b1111); $m |= 0b00010000; });
   }}

{package TX;
   our (@ISA); BEGIN { @ISA = (qw(T1_2 T2_1));
      SymObj::sym_create(0, { _ix => 'TX', _n => 'TX', _v => 1000 },
         sub { my ($self, $pkg) = @_; ::ok($m == 0b11111); $m |= 0b00100000; });
   }}

$m = 0;
$o = TX->new;
ok($m == 0b00111111);
ok($o->n eq 'TX' && $o->v == 1000 && $o->i1 eq 'T1_0' &&
   $o->i2 eq 'T1_1' && $o->i3 eq 'T1_2' && $o->i4 eq 'T2_0' &&
   $o->i5 eq 'T2_1');

# FIXME test without $Debug (optimized path) won't work due to _SymObj_ISA
# FIXME order, add a splitted (Y) hierarchy test (use TX as head of one super
# FIXME path, subclassed by TX1, then add another straight (or resue T1_ !)
# FIXME as second @ISA entry!  If that works, handling is right.

