#@ (S-)Sym(bolic)Obj(ect) - easy creation of classes and objects thereof.
package SymObj;
require 5.008_001;
$VERSION = '0.6.0b';
$COPYRIGHT =<<_EOT;
Copyright (c) 2010 - 2012 Steffen "Daode" Nurpmeso <sdaoden\@users.sf.net>.
All rights reserved under the terms of the ISC license.
_EOT
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
# 
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# We fool around with that by definition, so this
no strict 'refs';

BEGIN {
   #require Exporter;
   #@ISA = qw(Exporter);
   @EXPORT = qw(&pack_exists &sym_create &sym_dump
      &obj_ctor &obj_dump
      $VERSION $COPYRIGHT $Debug $Verbose);
}

$Debug = 1;
$Verbose = 0;
$NoFastCTors = 0;

sub _UUID { 'S-SymObj::1C8288D6-9EDA-4ECD-927F-2144B94186AD'; }

sub _complain_rdonly {
   return unless $SymObj::Debug;
   my ($pkg, $pub) = @_;
   print STDERR "${pkg}::$pub(): write access to readonly field rejected\n";
}

# TODO pack_ series incomplete (available in eval?? require??)
sub pack_exists {
   my ($pkg) = @_;
   defined %{"${pkg}::"};
}

sub sym_create {
   my ($pkg, $tfields, $fastctor) = @_;
   print STDERR "SymObj::sym_create(): $pkg\n" if $SymObj::Verbose;

   my ($i, $j, @renames, @isa, %socargs);

   # Minimize code-blow - offer some basic SymObj symtable entries.
   # Even that is overkill unless the %FIELDS really require that though.
   # Since we use these also for management tasks, then, do inject regardless
   # of wether the symbols are public or not.
   sub NEEDS_ARRAY   { 1<<0; }
   sub NEEDS_HASH    { 1<<1; }
   sub NEEDS_ALL     { 3; }

   $i = 0;
   foreach $j (keys %$tfields) {
      if (ref $tfields->{$j} eq 'ARRAY') {
         $i |= NEEDS_ARRAY;
         last if ($i & NEEDS_ALL) == NEEDS_ALL;
      } elsif (ref $tfields->{$j} eq 'HASH') {
         $i |= NEEDS_HASH;
         last if ($i & NEEDS_ALL) == NEEDS_ALL;
      }
   }

   if ($i & NEEDS_ARRAY) {
      print STDERR "\t..adding shared array handler\n" if $SymObj::Verbose;
      *{"${pkg}::_SymObj_ArraySet"} = sub {
         my ($self, $pub, $datum) = (shift, shift, shift);
         my $dref = $self->{$datum};
         # (Owed to lazy construction order)
         $dref = $self->{$datum} = [] unless defined $dref;

         foreach my $arg (@_) {
            if (ref $arg eq 'ARRAY') {
               push(@$dref, $_) foreach (@$arg);
            } elsif (ref $arg eq 'HASH') {
               while (my ($k, $v) = each %$arg) {
                  push @$dref, $k;
                  push @$dref, $v;
               }
            } else {
               push @$dref, $arg;
            }
         }
         $dref;
      };
   }

   if ($i & NEEDS_HASH) {
      print STDERR "\t..adding shared hash handler\n" if $SymObj::Verbose;
      *{"${pkg}::_SymObj_HashSet"} = sub {
         my ($self, $pub, $datum) = (shift, shift, shift);
         my $dref = $self->{$datum};
         # (Owed to lazy construction order)
         $dref = $self->{$datum} = {} unless defined $dref;

         my $k = undef;
         foreach my $arg (@_) {
            if (defined $k) {
               $dref->{$k} = $arg;
               $k = undef;
               next;
            }
            if (ref $arg eq 'HASH') {
               while (my ($k, $v) = each %$arg) {
                  $dref->{$k} = $v;
               }
            } elsif (ref $arg eq 'ARRAY') {
               while (@$arg > 1) {
                  my $v = pop @$arg;
                  my $k = pop @$arg;
                  $dref->{$k} = $v;
               }
               print STDERR "! ${pkg}::${pub}(): wrong array member count!\n"
                  if @$arg != 0 && $SymObj::Debug;
            } else {
               $k = $arg;
            }
         }
         print STDERR "! ${pkg}::${pub}(): '$k' key without a value\n"
            if defined $k && $SymObj::Debug;
         $dref;
      };
   }

   # Create accessor symtable entries
   foreach $j (keys %$tfields) {
      sub TYPE_EXCLUDE  { 1<<0; }
      sub TYPE_RDONLY   { 1<<1; }
      my ($xj, $pj, $tj) = ($j, $j, 0);

      # Don't create accessor subs?  Provide only read-only access?
      if ($pj =~ /^\?/) {
         $tj = TYPE_EXCLUDE;
      } elsif ($pj =~ /^!/) {
         $tj = TYPE_RDONLY;
      }
      if ($tj != 0) {
         $xj = $pj = substr $pj, 1;
         push @renames, [$j, $xj];
      }

      if ($pj =~ /^_/) {
         $pj = substr $pj, 1;
      } elsif ($SymObj::Debug) {
         print STDERR "\tSymbol '$pj': does NOT start with underscore _!\n";
      }
      $socargs{$pj} = $pj;

      # Always create a private accessor that returns a reference
      $i = ref $tfields->{$j};
      *{"${pkg}::__$pj"} = ($i eq 'ARRAY' || $i eq 'HASH')
         ? sub { $_[0]->{$xj}; } : sub { \$_[0]->{$xj}; };

      if ($tj == TYPE_EXCLUDE) {
         print STDERR "\tExclusion: not creating accessors for '$pj'\n"
            if $SymObj::Verbose;
         next;
      }

      # Instantiate 'em
      if ($i eq 'ARRAY') {
         print STDERR "\tsub $pj: array-based\n" if $SymObj::Verbose;
         *{"${pkg}::$pj"} = sub {
            my $self = $_[0];
            if (ref $self) { shift; }
            else           { $self = $tfields; }
            my $f = $self->{$xj};
            if (@_ > 0) {
               if ($tj == TYPE_RDONLY) {
                  SymObj::_complain_rdonly($pkg, $pj);
               } else {
                  $f = "${pkg}::_SymObj_ArraySet"->($self, $pj, $xj, @_);
               }
            }
            wantarray ? @$f : $f;
         };
      } elsif ($i eq 'HASH') {
         print STDERR "\tsub $pj: hash-based\n" if $SymObj::Verbose;
         *{"${pkg}::$pj"} = sub {
            my $self = $_[0];
            if (ref $self) { shift; }
            else           { $self = $tfields; }
            my $f = $self->{$xj};
            if (@_ > 0) {
               if ($tj == TYPE_RDONLY) {
                  SymObj::_complain_rdonly($pkg, $pj);
               } else {
                  $f = "${pkg}::_SymObj_HashSet"->($self, $pj, $xj, @_);
               }
            }
            wantarray ? %$f : $f;
         };
      } else {
         # Scalar (or "typeless")
         print STDERR "\tsub $pj: scalar-based (\"typeless\")\n"
            if $SymObj::Verbose;
         *{"${pkg}::$pj"} = sub {
            my $self = $_[0];
            if (ref $self) { shift; }
            else           { $self = $tfields; }
            if (@_ > 0) {
               if ($tj == TYPE_RDONLY) {
                  SymObj::_complain_rdonly($pkg, $pj);
               } else {
                  $self->{$xj} = shift;
               }
            }
            $self->{$xj};
         };
      }
   }

   # Refcopy all those fields which end up without a special prefix
   foreach $i (@renames) {
      $tfields->{$i->[1]} = $tfields->{$i->[0]};
      delete $tfields->{$i->[0]};
   }

   # We need a pointer to the fields for the constructor - overtake ownership
   *{"${pkg}::_SymObj_Fields"} = $tfields;

   if (defined $fastctor) {
      print STDERR "\tenabling fast-ctor code path\n" if $SymObj::Verbose;
      if (ref $fastctor ne 'CODE') {
         print STDERR "! ${pkg}: the passed \"fast constructor\" is not ",
            "a code reference!\n" if $SymObj::Debug;
         $fastctor = sub { print STDERR "${pkg}: invalid fast constructor\n"; }
      }
      *{"${pkg}::_SymObj_FastCTor"} = $fastctor;
      *{"${pkg}::_SymObj_FastCTorOk"} = 1;
   }

   # For (superior debug ctor) argument checking, create a hash of
   # public symbols (inherit those from parents first ...)
   if (defined ${"${pkg}::"}{"ISA"}) {
      foreach $i (@{*{"${pkg}::ISA"}}) {
         push @isa, $i;
         $j = *{"${i}::_SymObj_AllCTorArgs"};
         unless (defined $j) {
            print STDERR "! ${pkg}: \@ISA entry '$i' is not a SymObj managed ",
               "class!\n" if $SymObj::Debug;
            next;
         }
         foreach (keys %$j) {
            next if exists $socargs{$_};
            $socargs{$_} = $_;
         }

         if (defined $fastctor && ! defined ${"${i}::"}{"_SymObj_FastCTorOk"}) {
            print STDERR "\tdisabling fast-ctor, parent $i is incompatible\n"
               if $SymObj::Verbose;
            $fastctor = undef;
            delete ${"${pkg}::"}{"_SymObj_FastCTorOk"};
         }
      }
   }
   push @isa, $pkg;
   *{"${pkg}::_SymObj_ISA"} = \@isa;
   *{"${pkg}::_SymObj_AllCTorArgs"} = \%socargs;
}

sub sym_dump {
   my ($pkg) = @_;
   print STDERR "SymObj::sym_dump($pkg):\n\t";
   if (my $i = ref $pkg) { $pkg = $i; }
   foreach (keys %{*{"${pkg}::"}}) { print STDERR "$_ "; }
   #while (my ($k, $v) = each %{*{"${pkg}::"}}) { print STDERR "$k($v) "; }
   print STDERR "\n";
}

sub obj_ctor {
   my ($pkg, $class, $argaref, $over) = @_;
   print STDERR "SymObj::obj_ctor <> new: $pkg called as $class\n"
      if $SymObj::Verbose;

   my ($self, $tfields, $init_chain, $i, $j, $k) =
      (undef, *{"${pkg}::_SymObj_Fields"});

   # Since all classes use obj_ctor in their new, we would perform argument
   # checking over and over again; so use a savage and hacky, but
   # multithread-safe way to perform argument checking only in the ctor of
   # the real (actual sub-) class
   $init_chain = $SymObj::Debug && @$argaref > 0 && defined $argaref->[0] &&
      $argaref->[0] eq _UUID;

   # Inheritance handling
   if (defined(my $isa = *{"${pkg}::ISA"})) {
jOVER_OUTER:
      # Append overrides
      foreach $k (keys %$over) {
         for ($i = $init_chain, $j = @$argaref; $i < $j; $i += 2) {
            next jOVER_OUTER if $k eq $$argaref[$i];
         }
         push @$argaref, $k;
         push @$argaref, $over->{$k};
      }

      # Walk the new() chain,
      # but disallow arg-checking for superclasses
      unshift(@$argaref, _UUID) if $SymObj::Debug && ! $init_chain;

      foreach (@$isa) {
         unless (defined ${"${_}::"}{'new'}) {
            next unless $SymObj::Debug;
            print STDERR "${pkg}: $class->new(): no such package: $_!\n"
               and next unless defined *{"${_}::"};
            print STDERR "${pkg}: $class->new(): $_: misses a new() sub!\n";
            next;
         }
         $i = &{*{"${_}::new"}}($class, @$argaref);

         # (MI restriction applies here: if $self is yet
         # a {} the other tree can only be joined in and
         # thus looses it's hash-pointer)
         $self = $i and next unless defined $self;
         while (($k, $j) = each %$i) { $self->{$k} = $j; }
      }

      shift @$argaref if $SymObj::Debug && ! $init_chain;
   }

   # SELF
   $self = {} unless defined $self;
   $self = bless $self, $class;

   # Normal arguments; can and should we perform argument checking?
   if ($SymObj::Debug) {
      if ($init_chain) {
         shift @$argaref;
      } else {
         $over = *{"${pkg}::_SymObj_AllCTorArgs"};
         if (@$argaref & 1) {
            pop @$argaref;
            print STDERR "${pkg}: $class->new(): odd argument discarded!\n"
               if $SymObj::Debug;
         }
      }
   }

   # Use generic internal accessors here
   while (@$argaref) {
      $k = shift @$argaref;
      $i = '_' . $k;
      $j = shift @$argaref;

      my $tv = $tfields->{$i};
      unless (exists $tfields->{$i}) {
         next unless defined $over;
         next if exists $over->{$k};
         print STDERR "${pkg}: $class->new(): unknown argument: '$k'\n";
      } elsif (ref $tv eq 'ARRAY') {
         $self->_SymObj_ArraySet('new()', $i, $j);
      } elsif (ref $tv eq 'HASH') {
         unless (ref $v eq 'ARRAY' || ref $v eq 'HASH') {
            print STDERR "${pkg}: $class->new(): ",
               "'$k' requires ARRAY or HASH argument\n" if $SymObj::Debug;
            next;
         }
         $self->_SymObj_HashSet('new()', $i, $j);
      } else {
         $self->{$i} = $j;
      }
   }

   # Finally: fill in yet unset members of $self via the per-class template.
   # By default anon-hashes and -arrays get reference-copied;
   # we however *do* need a detached (deep) copy!
   while (($k, $j) = each %$tfields) {
      next if exists $self->{$k};
      unless (ref $j) {
         $self->{$k} = $j;
      } elsif (ref $j eq 'ARRAY') {
         my @a;
         push(@a, $_) foreach (@$j);
         $self->{$k} = \@a;
      } elsif (ref $j eq 'HASH') {
         my (%h, $hk, $hv);
         while (($hk, $hv) = each %$j) { $h{$hk} = $hv; }
         $self->{$k} = \%h;
      } elsif ($SymObj::Debug) {
         print STDERR "${pkg}: $class->new(): value of '$k' has an ",
            "unsupported type!\n";
      }
   }

   # Call further init code, if defined
   $tfields = "${pkg}::_SymObj_FastCTor";
   &$tfields($self) if defined *$tfields;
   $self;
}

sub obj_ctor_fast {
   my ($pkg, $class, $argaref, $over) = @_;
   return obj_ctor(@_) if ! defined ${"${pkg}::"}{"_SymObj_FastCTorOk"} ||
      $SymObj::NoFastCTors;

   # This class hierarchy is capable to perform simplified init!
   # Still, there is a lot to do..
   my $self = {};
   $self = bless $self, $class;
   my $isa = *{"${pkg}::_SymObj_ISA"};
   my $sym;

   # Embed arguments
   while (@$argaref) {
      my $k = shift @$argaref;
      my $pk = '_' . $k;
      my $v = shift @$argaref;

      foreach $pkg (@$isa) {
         $sym = *{"${pkg}::_SymObj_Fields"};
         next unless exists $sym->{$pk};
         my $tv = $sym->{$pk};
         if (ref $tv eq 'ARRAY') {
            $self->_SymObj_ArraySet('new()', $pk, $v);
         } elsif (ref $tv eq 'HASH') {
            $self->_SymObj_HashSet('new()', $pk, $v);
         } else {
            $self->{$pk} = $v;
         }
      }
   }

   # Fill what is not yet filled from arguments..
   foreach $pkg (@$isa) {
      $sym = *{"${pkg}::_SymObj_Fields"};
      while (my ($k, $v) = each %$sym) {
         next if exists $self->{$k};
         if (ref $v eq 'ARRAY') {
            my @a;
            push(@a, $_) foreach (@$v);
            $self->{$k} = \@a;
         } elsif (ref $v eq 'HASH') {
            my (%h, $hk, $hv);
            while (($hk, $hv) = each %$v) { $h{$hk} = $hv; }
            $self->{$k} = \%h;
         } else {
            $self->{$k} = $v;
         }
      }
      &{*{"${pkg}::_SymObj_FastCTor"}}($self);
   }

   $self;
}

sub obj_dump {
   use Data::Dumper;
   my $self = shift;
   print STDERR "SymObj::obj_dump(): ", Dumper($self), "\n";
}

1;
__END__

=head1 S-SymObj

SymObj.pm provides an easy way to create and construct symbol-tables
and objects.  With a simple hash one defines class-fields an object
should have.  A generic constructor will then create the object and
all of its superclasses, checking and filtering arguments along the
way, which makes it pretty useful in times when the interface is
unstable.  The generated accessor subs which are created for arrays
and hashes implement a I<feed in and forget> approach, since they can
handle all kinds of arguments (or try it); this is also true for the
constructor.

SymObj.pm works for Multiple-Inheritance as good as perl(1) allows.
(That is to say that only one straight object tree can be used, further
trees of C<@ISA> need to be joined into the C<$self> hash and thus
loose I<their> C<$self> along the way, of course.)  It should integrate
neatlessly into SMP in respect to objects; package "static-data" however
is not protected.

The S-SymObj project is located at
L<https://sourceforge.net/projects/ssymobj>; since that is a SourceForge
Beta project page, L<http://sdaoden.users.sourceforge.net/code.html>
is maybe more interesting.  S-SymObj is developed using a git(1)
repository, which is located at C<git.code.sf.net/p/ssymobj/code>.

=head2 Usage example

   BEGIN {
      require 'SymObj.pm';       
      ($SymObj::Debug, $SymObj::Verbose, $SymObj::NoFastCTors) = (1, 0, 0);
   }

   ## "Feed-in and forget" ##

   {package X_Super;
      BEGIN {
         SymObj::sym_create(__PACKAGE__, {
               _name => '',
               _array => [qw(av1 av2)],
               _hash => {hk1 => 'hv1', hk2 => 'hv2'}
            });
      }
      sub new { SymObj::obj_ctor(__PACKAGE__, shift, \@_); }
   }

   {package SomePack;
      our (@ISA);
      BEGIN {
         @ISA = ('X_Super');
         SymObj::sym_create(__PACKAGE__, {}); # <- adds no fields on its own
      }
      sub new { SymObj::obj_ctor(__PACKAGE__, shift, \@_); }
   }

   my $sp = SomePack->new(name => 'SymObj is easy', 'unknown' => 'arg');

   # Other possible ctor arguments to init ->array() and ->hash()
   # array => '1' # (Need to push the rest later on)
   # array => ['1', '2', '3', '4']
   # array => [qw(1 2 3 4)]
   # array => {1 => '2', 3 => '4'}
   # hash => [qw(i you we all)]
   # hash => {i => 'you', we => 'all'}

   # The accessor subs also try to swallow everything.
   # They return references, except for scalars (always) and in wantarray
   # context, in which case you get a copy
   my $v = $sp->name('SymObj is really nice to use');
   print "name is <$v>\n";

   my $vr;
   $vr = $sp->array(   '1_1'); $sp->array('2_1');
   $vr = $sp->array(qw( 1_2                2_2));
   $vr = $sp->array([qw(1_3                2_3)]);
   $vr = $sp->array(   '1_4' =>           '2_4');
   my @arrcopy = $sp->array(); # wantarray context gives copy instead

   $vr = $sp->hash(    i_1 => 'you',  we_1 => 'all');
   $vr = $sp->hash(   'i_2',  'you', 'we_2',  'all');
   $vr = $sp->hash(qw( i_3    you     we_3     all));
   $vr = $sp->hash([qw(i_4    you     we_4     all)]);
   $vr = $sp->hash({   i_5 => 'you',  we_5 => 'all'});
   my %hashcopy = $sp->hash(); # wantarray context gives copy instead

   SymObj::obj_dump($sp);

   ## Adjust the per-class template (but don't do that) ##

   undef %{X_Super::hash()};
   X_Super::hash(newhk1=>'newhv1', newhk2=>'newhv2');
   $sp = SomePack->new(name => 'SymObj is really easy');
   SymObj::obj_dump($sp);

   ## "Fast-constructor" path, no/readonly accessors ##

   {package FC1;
      BEGIN {
         SymObj::sym_create(__PACKAGE__, { '?_n' => 'FC1', '!_v' => '1' },
            sub {
               my $self = shift;
               print ".. FC1::fast-ctor (", $self->{_n}, ")\n";
            });
      }
      sub new { SymObj::obj_ctor_fast(__PACKAGE__, shift, \@_); }
   }
   {package FC2;
      our (@ISA);
      BEGIN {
         @ISA = ('FC1');
         SymObj::sym_create(__PACKAGE__, { '?_n' => 'FC2', '!_v' => '2' },
            sub {
               my $self = shift;
               print ".. FC2::fast-ctor (", $self->{_n}, ")\n";
            });
      }
      sub new { SymObj::obj_ctor_fast(__PACKAGE__, shift, \@_); }
   }

   my $i = FC2->new;
   print "i->v = ", $i->v(), "\n";
   $i->v('it is readonly..');
   $i = FC2->new(v=>3);
   print "i->v = ", $i->v(), "\n";
   #$i->n('no accessor for this at all..');

=head2 Package-Symbols

=over

=item C<$VERSION> (string, i.e., '0.6.0b')

A version string.

=item C<$COPYRIGHT> (string)

A multiline string which contains a summary of the copyright license.
S-SymObj is provided under the terms of the ISC license.

=item C<$Debug> (boolean, i.e., 0 or 1, default 1)

Indicates wether some checks etc. shall be performed or not.  By default
enabled.  Messages go to STDERR.

=item C<$Verbose> (boolean, i.e., 0 or 1, default 0)

If enabled some more informational etc. messages go to STDERR.
By default disabled.

=item C<$NoFastCTors> (boolean, i.e., 0 or 1, default 0)

Can be used to thoroughly and completely disable usage of fast
constructors, as described below.  If set these will always propagate
to normal constructor calls.

=item C<pack_exists(C<$1>=string=package/class)>

Check wether the class (package) $1 exists.

=item C<sym_create($1=string=package, $2=hash-ref=fields[, $3=code-ref])>

Create accessor methods/functions in the C<__PACKAGE__> C<$1> for all
keys of C<$2> I<and> do some more magic symbol table housekeeping to
make SymObj work.  Due to the additional magic this must be called
even if no fields exist, yet even if C<$2> is the empty anonymous C<{}>
hash (as shown above).

Note that C<$2> is internally mirrored, but I<not> deep copied.
It should be assumed that ownership of C<$2> is overtaken by S-SymObj.

SymObj generally "enforces" privacy (by definition) via an underscore
prefix: all keys of C<$2> are expected to start with an underscore,
but the public accessor methods will miss that (C<_data> becomes
C<data>).

The created accessor subs work as methods if a C<$self> object exists
(as in C<$self-E<gt>name()>) and as functions otherwise
(C<SomePack::name()>), in which case the provided package template
hash (C<$2>) is used!  (Note that no locking is performed in the latter
case, i.e., this should not be done in multithreaded programs.)
If they act upon arrays or hashs they'll return references to the
members by default, but do return copies in C<wantarray> context
instead.

If a key in C<$2> is prefixed with a question mark, as in C<'?_name'>,
then this means that no accessor subs will be created for C<name>.
The mark will be stripped internally, i.e., the member is C<_name>,
as expected, and that's also the way it is handled otherwise.

Just likewise, if a key in C<$2> are prefixed with an exclamation mark,
as in C<'!_name'>, then there will only be a readonly accessor sub be
made available.  Write access will be rejected, and also logged if
C<$SymObj::Debug> is true.

In addition to those accessor subs there will I<always> be private
accessor subs be created which use the public name prefixed with two
underscores, as in C<$self-E<gt>__name()>.  These subs do nothing
except returning a reference to the field in question.  They're ment
to be used instead of direct access of members in some contexts, i.e.,
for encapsulation purposes.

The optional third argument must be a code reference, i.e., either an
anonymous closure sub or the reference to a yet defined sub.  If given,
it enables the so-called "fast constructor" path for the object which
is yet defined.  The code reference will be called at the end of object
construction, taking one argument, the new C<$self> object, and returning
no value.  Please see L<"obj_ctor_fast"> below for more.

=item C<sym_dump($1=string OR object=symbol table target)>

Dump the symbol table entries of the package or object C<$1>.

=item C<obj_ctor($1=string=package, $2=$self=class, $3=array-ref=arguments, [$4=hash-ref=overrides])>

Create self (and it's superclass-instances).  C<$3> I<is> C<@_>, i.e.,
the reference to the ctors own arguments.  The optional C<$4> argument
is only required if the class has superclasses and needs to regulary
override some of the values of these; it is a hash which defines the
key/value tuples to be overwritten: these will be merged into C<$3>
if, and only if, they are not yet contained therein; note that the
hash is ignored unless there really is a C<@ISA>, and that it's keys
must refer to public names.

=item C<obj_ctor_fast($1=string=package, $2=$self=class, $3=array-ref=arguments, [$4=hash-ref=overrides])>

This is identical to L<"obj_ctor"> unless all superclasses (if any)
of C<$self> have the so-called "fast-constructor" path enabled and the
L<"NoFastCTors"> flag isn't set.

Otherwise it implements a very fast constructor, which doesn't check
arguments, which doesn't perform full object creation and which doesn't
even call C<new()> for the superclasses in C<@ISA>.

Instead it'll simply create the hash with the provided arguments, join
in all missing fields of all superclasses, and finally calls the
fast-constructor code reference for the entire class hierarchy in
typical perl(1) order.  I.e., this will only work if the normal C<new()>
of all classes simply call this function ...

=item C<obj_dump($1=$self)>

Dumper::dump.

=back

=cut
# vim:set fenc=utf-8 filetype=perl syntax=perl ts=8 sts=3 sw=3 et tw=79:
