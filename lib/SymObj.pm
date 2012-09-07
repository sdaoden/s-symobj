#@ (S-)Sym(bolic)Obj(ect) - easy creation of symbol tables and objects.
package SymObj;
require 5.008_001;
our $VERSION = '0.6.1';
our $COPYRIGHT =<<__EOT__;
Copyright (c) 2010 - 2012 Steffen "Daode" Nurpmeso <sdaoden\@users.sf.net>.
All rights reserved under the terms of the ISC license.
__EOT__
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

use diagnostics -verbose;
use warnings;
use strict;
no strict 'refs'; # We fool around with that by definition, so this

sub NONE()        { 0 }
sub DEBUG()       { 1<<0 }
sub VERBOSE()     { 1<<1 }
sub _USRMASK()    { 0x7 }

sub _HAS_ARRAY()  { 1<<3 }
sub _HAS_HASH()   { 1<<4 }
sub _HAS_ALL()    { _HAS_ARRAY | _HAS_HASH }

sub _CLEANHIER()  { 1<<5 }

sub _UUID         { 'S-SymObj::1C8288D6-9EDA-4ECD-927F-2144B94186AD'; }

our $MsgFH = *STDERR;
our $Debug = 1; # 0,1,2

sub pack_exists {
   %{"${_[0]}::"};
}

sub sym_dump {
   my ($pkg) = @_;
   print $MsgFH "SymObj::sym_dump($pkg):\n\t";
   if (my $i = ref $pkg) { $pkg = $i; }
   foreach (keys %{*{"${pkg}::"}}) { print $MsgFH "$_ "; }
   #while (my ($k, $v) = each %{*{"${pkg}::"}}) { print $MsgFH "$k($v) "; }
   print $MsgFH "\n";
}

sub obj_dump {
   use Data::Dumper;
   my $self = shift;
   print $MsgFH "SymObj::obj_dump(): ", Dumper($self), "\n";
}

sub sym_create { # {{{
   my ($flags, $tfields, $ctor) = @_;
   $flags &= _USRMASK;
   $flags |= DEBUG if $flags & VERBOSE;
   $flags |= $Debug > 1 ? (DEBUG | VERBOSE) : DEBUG if $Debug;
   my ($pkg, $i, $j, @isa, @arri, %actorargs, @ctorovers) =
      (scalar caller, $flags & VERBOSE);
   print $MsgFH "SymObj::sym_create(): $pkg\n" if $i;

   # For (superior debug ctor) argument checking, create a hash of
   # public symbols (inherit those from parents first ...)
   # Note that our @isa<->*_SymObj_ISA is flattened, in construction order
   $flags |= _CLEANHIER;
   _resolve_tree($pkg, \%actorargs, $pkg, \$flags, \@isa)
      if defined ${"${pkg}::"}{ISA};
   push @isa, $pkg;

   print $MsgFH ".. (inherited VERBOSE:) SymObj::sym_create(): $pkg\n"
      if ($flags & VERBOSE) && ! $i;

   # Accessor and $tfields handling {{{
   # We use shared per-object field handler subs to minimize code blow a bit.
   # Even that is overkill unless the %FIELDS really require that though.
   @arri = keys %$tfields;
   foreach $i (@arri) {
      $j = ref $tfields->{$i};
      if ($i =~ /^@/ || $j eq 'ARRAY') {
         $flags |= _HAS_ARRAY;
         last if ($flags & _HAS_ALL) == _HAS_ALL;
      } elsif ($i =~ /^%/ || $j eq 'HASH') {
         $flags |= _HAS_HASH;
         last if ($flags & _HAS_ALL) == _HAS_ALL;
      }
   }

   if ($flags & _HAS_ARRAY) {
      print $MsgFH "\t..adding shared array handler\n" if $flags & VERBOSE;
      *{"${pkg}::_SymObj_ArraySet"} = sub {
         my ($self, $pub, $datum) = (shift, shift, shift);
         my $dref = $self->{$datum};
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

   if ($flags & _HAS_HASH) {
      print $MsgFH "\t..adding shared hash handler\n" if $flags & VERBOSE;
      *{"${pkg}::_SymObj_HashSet"} = sub {
         my ($self, $pub, $datum) = (shift, shift, shift);
         my $dref = $self->{$datum};
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
               print $MsgFH "! ${pkg}::$pub(): wrong array member count!\n"
                  if @$arg != 0 && ($flags & DEBUG);
            } else {
               $k = $arg;
            }
         }
         print $MsgFH "! ${pkg}::$pub(): '$k' key without a value\n"
            if defined $k && ($flags & DEBUG);
         $dref;
      };
   }

   # Create accessor symtable entries
   foreach $j (@arri) {
      sub TYPE_ARRAY()     { 1<<0 }
      sub TYPE_HASH()      { 1<<1 }
      sub TYPE_EXCLUDE()   { 1<<2 }
      sub TYPE_RDONLY()    { 1<<3 }
      my ($xj, $pj, $tj) = ($j, $j, 0);

      $j =~ /^([@%])?([?!])?(_)?(.*)/;
      $tj |= $1 eq '@' ? TYPE_ARRAY : TYPE_HASH if defined $1;
      $tj |= $2 eq '?' ? TYPE_EXCLUDE : TYPE_RDONLY if defined $2;
      print $MsgFH "\tSymbol '$pj': does NOT start with underscore _!\n"
         if ! defined $3 && ($flags & DEBUG);
      unless (defined $4 && length $4) {
         print $MsgFH "\tSymbol '$pj': consists only of modifier and/or ",
            "typedef and/or underscore!  Skip!!\n" if $flags & DEBUG;
         delete $tfields->{$j};
         next;
      }
      $xj = (defined $3 ? $3 : '') . $4;
      $pj = $4;

      if ($tj) {
         $tfields->{$xj} = $tfields->{$j};
         delete $tfields->{$j};
         $i = ($tj & TYPE_ARRAY) ? 'ARRAY' : ($tj & TYPE_HASH) ? 'HASH' : '';
      } else {
         $i = ref $tfields->{$xj};
      }

      # Does any superclass define this symbol already, i.e., is this an
      # override request?
      if (exists $actorargs{$pj}) {
         if ($i ne ref ${$actorargs{$pj}}) {
            print $MsgFH "${pkg}: overrides '$pj' of some super class ",
               "with incompatible type!  Skip!!\n" if $flags & DEBUG;
            delete $tfields->{$xj};
            next;
         }
         push @ctorovers, $pj;
         # And we do have to override the function(/method) too..
      }
      $actorargs{$pj} = \$tfields->{$xj};

      # Hope that perl(1) optimizes away the TYPE_* conditions..
      if (($tj & TYPE_ARRAY) || $i eq 'ARRAY') {
         print $MsgFH "\tsub $pj: array-based\n" if $flags & VERBOSE;
         *{"${pkg}::__$pj"} = sub { $_[0]->{$xj}; };
         *{"${pkg}::$pj"} = sub {
            my $self = $_[0];
            if (($self = ref $self) ne '' && %{"${self}::"}) {
               $self = shift;
               if ($tj & TYPE_EXCLUDE) {
                  SymObj::_complain_exclude($pkg, $pj) if $flags & DEBUG;
                  $self = $tfields;
               }
            } else {
               $self = $tfields;
            }
            my $f = $self->{$xj};
            if (! defined $f || @_) {
               SymObj::_complain_rdonly($pkg, $pj)
                  if ($tj & TYPE_RDONLY) && @_ && ($flags & DEBUG);
               $f = "${pkg}::_SymObj_ArraySet"->($self, $pj, $xj, @_);
            }
            wantarray ? @$f : $f;
         };
      } elsif (($tj & TYPE_HASH) || $i eq 'HASH') {
         print $MsgFH "\tsub $pj: hash-based\n" if $flags & VERBOSE;
         *{"${pkg}::__$pj"} = sub { $_[0]->{$xj}; };
         *{"${pkg}::$pj"} = sub {
            my $self = $_[0];
            if (($self = ref $self) ne '' && %{"${self}::"}) {
               $self = shift;
               if ($tj & TYPE_EXCLUDE) {
                  SymObj::_complain_exclude($pkg, $pj) if $flags & DEBUG;
                  $self = $tfields;
               }
            } else {
               $self = $tfields;
            }
            my $f = $self->{$xj};
            if (! defined $f || @_) {
               SymObj::_complain_rdonly($pkg, $pj)
                  if ($tj & TYPE_RDONLY) && @_ && ($flags & DEBUG);
               $f = &{${"${pkg}::"}{_SymObj_HashSet}}($self, $pj, $xj, @_);
            }
            wantarray ? %$f : $f;
         };
      } else {
         # Scalar (or "typeless")
         print $MsgFH "\tsub $pj: scalar-based ('untyped')\n"
            if $flags & VERBOSE;
         *{"${pkg}::__$pj"} = sub { \$_[0]->{$xj}; };
         *{"${pkg}::$pj"} = sub {
            my $self = $_[0];
            if (($self = ref $self) ne '' && %{"${self}::"}) {
               $self = shift;
               if ($tj & TYPE_EXCLUDE) {
                  SymObj::_complain_exclude($pkg, $pj) if $flags & DEBUG;
                  $self = $tfields;
               }
            } else {
               $self = $tfields;
            }
            if (@_) {
               SymObj::_complain_rdonly($pkg, $pj)
                  if ($tj & TYPE_RDONLY) && ($flags & DEBUG);
               $self->{$xj} = shift;
            }
            $self->{$xj};
         };
      }
   }
   # }}}

   # Variable fields
   ${"${pkg}::"}{_SymObj_PACKAGE} = $pkg;
   ${"${pkg}::"}{_SymObj_ISA} = \@isa;
   ${"${pkg}::"}{_SymObj_ALL_CTOR_ARGS} = \%actorargs;
   ${"${pkg}::"}{_SymObj_CTOR_OVERRIDES} = \@ctorovers;
   ${"${pkg}::"}{_SymObj_FIELDS} = $tfields;
   ${"${pkg}::"}{_SymObj_FLAGS} = $flags;

   # User constructor?
   if (! defined $ctor || ref $ctor ne 'CODE') {
      my $_c = $ctor;
      $_c = '__ctor' unless defined $_c;
      $ctor = sub { SymObj::_find_usr_ctor(shift, $pkg, $_c); };
   }
   ${"${pkg}::"}{_SymObj_USR_CTOR} = $ctor;

   # new()
   if ($flags & DEBUG) {
      $ctor = sub { SymObj::_ctor_dbg($pkg, shift, \@_); };
   } elsif ($flags & _CLEANHIER) {
      $ctor = sub { SymObj::_ctor_cleanhier($pkg, shift, \@_); };
   } else {
      $ctor = sub { SymObj::_ctor_dirtyhier($pkg, shift, \@_); };
   }
   *{"${pkg}::new"} = $ctor;
} # }}}

sub _ctor_dbg { # {{{
   my ($pkg, $class, $argaref) = @_;
   my $flags = ${"${pkg}::"}{_SymObj_FLAGS};
   print $MsgFH "SymObj::obj_ctor <> new: $pkg called as $class\n"
      if $flags & VERBOSE;

   my ($self, $actorargs, $ctorovers, $tfields, $init_chain, $i, $j, $k) =
      (undef, ${"${pkg}::"}{_SymObj_ALL_CTOR_ARGS},
      ${"${pkg}::"}{_SymObj_CTOR_OVERRIDES}, ${"${pkg}::"}{_SymObj_FIELDS});

   # Use a savage and hacky but multithread-safe way to perform argument
   # checking only in the ctor of the real (actual sub-) class (a.k.a. once)
   $init_chain = @$argaref > 0 && defined $argaref->[0] &&
      $argaref->[0] eq _UUID;

   # Inheritance handling
   if (defined ${"${pkg}::"}{ISA}) {
      # Append overrides
      foreach $k (@$ctorovers) {
         for ($i = $init_chain, $j = @$argaref; $i < $j; $i += 2) {
            goto j_OVW if $k eq $$argaref[$i];
         }
         push @$argaref, $k;
         push @$argaref, $tfields->{'_' . $k};
j_OVW:}

      # Walk the new() chain, but disallow arg-checking for superclasses
      unshift @$argaref, _UUID if ! $init_chain;

      foreach my $c (@{${"${pkg}::"}{ISA}}) {
         unless (defined ${"${c}::"}{new}) {
            print $MsgFH "${pkg}: $class->new(): no such package: $c!\n"
               and next unless %{"${c}::"};
            print $MsgFH "${pkg}: $class->new(): $c: misses a new() sub!\n";
            next;
         }
         $i = &{${"${c}::"}{new}}($class, @$argaref);

         # (MI restriction applies here: if $self is yet a {} the other tree
         # can only be joined in and thus looses it's hash-pointer)
         $self = $i and next unless defined $self;
         while (($k, $j) = each %$i) { $self->{$k} = $j; }
      }

      shift @$argaref if ! $init_chain;
   }

   # SELF
   $self = {} unless defined $self;
   $self = bless $self, $class;

   # Normal arguments; can and should we perform argument checking?
   shift @$argaref if $init_chain;
   if (@$argaref & 1) {
      pop @$argaref;
      print $MsgFH "${pkg}: $class->new(): odd argument discarded!\n";
   }

   # Use generic internal accessors here
   while (@$argaref) {
      $k = shift @$argaref;
      $i = '_' . $k;
      $j = shift @$argaref;
      unless (exists $actorargs->{$k}) {
         next if $init_chain;
         print $MsgFH "${pkg}: $class->new(): unknown argument: '$k'\n";
         next;
      }
      next unless exists $tfields->{$i};

      if (ref $tfields->{$i} eq 'ARRAY') {
         $self->_SymObj_ArraySet('new()', $i, $j);
      } elsif (ref $tfields->{$i} eq 'HASH') {
         unless (ref $j eq 'ARRAY' || ref $j eq 'HASH') {
            print $MsgFH "${pkg}: $class->new(): ",
               "'$k' requires ARRAY or HASH argument\n";
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
         push @a, $_ foreach (@$j);
         $self->{$k} = \@a;
      } elsif (ref $j eq 'HASH') {
         my (%h, $hk, $hv);
         while (($hk, $hv) = each %$j) { $h{$hk} = $hv; }
         $self->{$k} = \%h;
      } else {
         print $MsgFH "${pkg}: $class->new(): value of '$k' has an ",
            "unsupported type!\n";
      }
   }

   # Call further init code, if defined
   if (defined($i = ${"${pkg}::"}{_SymObj_USR_CTOR})) {
      &$i($self);
   }
   $self;
} # }}}

sub _ctor_dirtyhier { # (Stripped version of _dbg) {{{
   my ($pkg, $class, $argaref) = @_;

   my ($self, $actorargs, $ctorovers, $tfields, $i, $j, $k) =
      (undef, ${"${pkg}::"}{_SymObj_ALL_CTOR_ARGS},
      ${"${pkg}::"}{_SymObj_CTOR_OVERRIDES}, ${"${pkg}::"}{_SymObj_FIELDS});

   if (defined ${"${pkg}::"}{ISA}) {
      foreach $k (@$ctorovers) {
         for ($i = 0, $j = @$argaref; $i < $j; $i += 2) {
            goto j_OVW if $k eq $$argaref[$i];
         }
         push @$argaref, $k;
         push @$argaref, $tfields->{'_' . $k};
j_OVW:}

      foreach my $c (@{${"${pkg}::"}{ISA}}) {
         unless (defined ${"${c}::"}{new}) {
            next;
         }
         $i = &{${"${c}::"}{new}}($class, @$argaref);

         $self = $i and next unless defined $self;
         while (($k, $j) = each %$i) { $self->{$k} = $j; }
      }
   }

   $self = {} unless defined $self;
   $self = bless $self, $class;

   while (@$argaref) {
      $k = shift @$argaref;
      $i = '_' . $k;
      $j = shift @$argaref;
      unless (exists $actorargs->{$k}) {
         next;
      }
      next unless exists $tfields->{$i};

      if (ref $tfields->{$i} eq 'ARRAY') {
         $self->_SymObj_ArraySet('new()', $i, $j);
      } elsif (ref $tfields->{$i} eq 'HASH') {
         unless (ref $j eq 'ARRAY' || ref $j eq 'HASH') {
            next;
         }
         $self->_SymObj_HashSet('new()', $i, $j);
      } else {
         $self->{$i} = $j;
      }
   }

   while (($k, $j) = each %$tfields) {
      next if exists $self->{$k};
      unless (ref $j) {
         $self->{$k} = $j;
      } elsif (ref $j eq 'ARRAY') {
         my @a;
         push @a, $_ foreach (@$j);
         $self->{$k} = \@a;
      } elsif (ref $j eq 'HASH') {
         my (%h, $hk, $hv);
         while (($hk, $hv) = each %$j) { $h{$hk} = $hv; }
         $self->{$k} = \%h;
      }
   }

   if (defined($i = ${"${pkg}::"}{_SymObj_USR_CTOR})) {
      &$i($self);
   }
   $self;
} # }}}

sub _ctor_cleanhier { # {{{
   my ($pkg, $class, $argaref) = @_;
   my $self = {};
   $self = bless $self, $class;
   my $isa = ${"${pkg}::"}{_SymObj_ISA};
   my $allargs = ${"${pkg}::"}{_SymObj_ALL_CTOR_ARGS};

   # Embed arguments
   while (@$argaref) {
      my $k = shift @$argaref;
      my $pk = '_' . $k;
      my $v = shift @$argaref;
      my $tv = ${$allargs->{$k}};
      if (ref $tv eq 'ARRAY') {
         $self->_SymObj_ArraySet('new()', $pk, $v);
      } elsif (ref $tv eq 'HASH') {
         $self->_SymObj_HashSet('new()', $pk, $v);
      } else {
         $self->{$pk} = $v;
      }
   }

   # Fill what is not yet filled from arguments..
   while (my ($k, $v) = each %$allargs) {
      $k = '_' . $k;
      next if exists $self->{$k};
      $v = $$v;
      if (ref $v eq 'ARRAY') {
         my @a;
         push @a, $_ foreach (@$v);
         $self->{$k} = \@a;
      } elsif (ref $v eq 'HASH') {
         my (%h, $hk, $hv);
         while (($hk, $hv) = each %$v) { $h{$hk} = $hv; }
         $self->{$k} = \%h;
      } else {
         $self->{$k} = $v;
      }
   }

   # Call user CTORs in correct order..
   foreach $pkg (@$isa) {
      if (defined(my $sym = ${"${pkg}::"}{_SymObj_USR_CTOR})) {
         &$sym($self);
      }
   }
   $self;
} # }}}

sub _resolve_tree { # {{{
   my ($pkg, $_actorargs, $_p, $_f, $_isa) = @_;
   foreach my $c (@{${"${_p}::"}{ISA}}) {
      unless (%{"${c}::"}) {
         print $MsgFH "${pkg}: $_p: \@ISA contains non-existent ",
            "class '$c'!\n" if $$_f & DEBUG;
         next;
      }

      my $j = ${"${c}::"}{_SymObj_FLAGS};
      unless (defined $j) {
         print $MsgFH "${pkg}: $_p:  '$c' not SymObj managed: hierarchy not ",
            "clean, STOP!\n" if $$_f & VERBOSE;
         $$_f &= ~_CLEANHIER;
         next;
      }
      $$_f |= $j & (DEBUG | VERBOSE); # Inherit debug states
      if (! ($j & _CLEANHIER) && ($$_f & _CLEANHIER)) {
         $$_f &= ~_CLEANHIER;
         print $MsgFH "${pkg}: $_p: '$c' says hierarchy is not clean..\n"
            if $$_f & VERBOSE;
      }

      while (my ($k, $v) = each %{${"${c}::"}{_SymObj_ALL_CTOR_ARGS}}) {
         $_actorargs->{$k} = $v;
      }

      _resolve_tree($pkg, $_actorargs, $c, $_f, $_isa)
         if defined ${"${c}::"}{ISA};
      push @$_isa, $c;
   }
} # }}}

sub _complain_exclude {
   my ($pkg, $pub) = @_;
   print $MsgFH "${pkg}::$pub(): field can't be accessed through object, ",
      "accessing class-static!\n";
}

sub _complain_rdonly {
   my ($pkg, $pub) = @_;
   print $MsgFH "${pkg}::$pub(): write access to READONLY field!\n";
}

sub _find_usr_ctor { # {{{
   # No constructor was given to sym_create(), or it was no code-ref.
   # Try to find out what the user wants.
   my ($self, $pkg, $ctor) = @_;
   my $flags = ${"${pkg}::"}{_SymObj_FLAGS};
   unless (defined ${"${pkg}::"}{$ctor}) {
      print $MsgFH "${pkg}: no user ctor\n" if $flags & VERBOSE;
      delete ${"${pkg}::"}{_SymObj_USR_CTOR};
   } else {
      print $MsgFH "${pkg}: resolved user ctor '$ctor'\n" if $flags & VERBOSE;
      $ctor = ${"${pkg}::"}{$ctor};
      ${"${pkg}::"}{_SymObj_USR_CTOR} = $ctor;
      &$ctor($self);
   }
} # }}}

1;
__END__
# POD {{{

=head1 NAME

S-SymObj -- an easy way to create symbol-tables and objects.

=head1 SYNOPSIS

   # You need to require it in a BEGIN{}..; try out $Debug= 1/2
   BEGIN { require SymObj; $SymObj::Debug = 0; }

   # Accessor subs return references for hashes and arrays (but shallow
   # copy in wantarray context) scalars are returned "as-is"
   {package X1;
      BEGIN {
         SymObj::sym_create(SymObj::NONE, { # (NONE is 0..)
               _name => '', _array => [qw(Is Easy)],
               _hash => {To => 'hv1', Use => 'hv2'} });
      }
   }
   my $o = X1->new(name => 'SymObj');
   print $o->name, ' ';
   print join(' ', @{$o->array}), ' ';
   print join(' ', keys %{$o->hash}), "\n";

   # Unknown arguments are detected when DEBUG/VERBOSE is enabled.
   {package X2;
      our (@ISA); BEGIN { @ISA = ('X1');
         SymObj::sym_create(0, {}); # <- adds no fields on its own
      }
   }
   my $o = X2->new(name => 'It detects some misuses', 'un' => 'known');
   print $o->name, "\n";

   # Fields which mirror fieldnames of superclasses define overrides.
   {package X3;
      our (@ISA); BEGIN { @ISA = ('X2');
         SymObj::sym_create(0, { '_name' => 'Auto superclass-ovw'},
            sub { my $self = shift; print "X3 usr ctor\n"; });
      }
   }
   $o = X3->new();
   print $o->name, "\n";

   # One may enforce creation of array/hash accessors even for undef
   # values by using the @/% type modifiers; the objects themselves
   # are lazy-created as necessary, then...
   {package X4;
      our (@ISA); BEGIN { @ISA = ('X3');
         SymObj::sym_create(0, { '%_hash2'=>undef, '@_array2'=>undef });
      }
      sub __ctor { my $self = shift; print "X4 usr ctor\n"; }
   }
   $o = X4->new(name => 'A X4');
   die 'Lazy-allocation failed'
      if ! defined $o->hash2 || ! defined $o->array2;
   print join(' ', keys %{$o->hash2(Allocation=>1, Lazy=>1)}), ' ';
   print join(' ', @{$o->array2(qw(Can Be Used))}), "\n";

   %{$o->hash2} = ();
   $o->hash2(HashAndArray => 'easy');
   $o->hash2(qw(Accessors development));
   $o->hash2('Really', 'is');
   $o->hash2(['Swallow', 'possible']);
   $o->hash2({ Anything => 'here' });
   print join(' ', keys %{$o->hash2}), "\n";
   # P.S.: this is also true for the constructor(s)

=head1 DESCRIPTION

SymObj.pm provides an easy way to create and construct symbol-tables
and objects.  With a simple hash one defines the fields an object
should have.  An automatically instantiated constructor can then be
used to create the object, and the generated accessor subs implement
a I<feed in and forget> approach when they are about to manage arrays
or hashes, trying to handle all kinds of arguments; this is also true
for the constructor.

If debug was enabled upon creation time a constructor which does a lot
of argument checking and more is used, which is pretty useful in times
when the interface is unstable.  Otherwise a different constructor is
used which implements no checking at all; and if the object in question
is the head of a "clean" object tree, one that is entirely managed by
S-SymObj, then indeed a super-fast super-lean constructor implementation
is used that'll rock your house.

SymObj.pm works for Multiple-Inheritance as good as perl(1) allows.
(That is to say that only one straight object tree can be used, further
trees of C<@ISA> need to be joined into the C<$self> hash and thus
loose I<their> C<$self> along the way, of course.)  It should integrate
neatlessly into SMP in respect to objects; package "static-data" however
is not protected.  Note that S-SymObj does I<not> add tweaks to the
perl(1) object mechanism in respect to superclasses that occur multiple
times in the C<@ISA> of some class; this is because the resulting
behaviour would differ for clean S-SymObj managed and mixed trees, as
well as for debug and non-debug mode (though that could be managed,
actually).

B<Note> that it is not possible to use an object tree with mixed
S-SymObj managed and non-managed classes in B<mixed order>, as in
I<MANAGED subclassof NON-MANAGED subclassof MANAGED>, because tree
traversal actually stops once a I<NON-MANAGED> class is seen.  This
is logical, because non-managed classes do not contain the necessary
information for S-SymObj.

The SymObj module is available on CPAN.  The S-SymObj project is located
at L<http://sdaoden.users.sourceforge.net/code.html>.  It is developed
using a git(1) repository, which is located at
C<git.code.sf.net/p/ssymobj/code> (or browse it at
L<http://sourceforge.net/p/ssymobj/code/>).

=head1 INTERFACE

=over

=item C<$VERSION> (string, i.e., '0.6.1')

A version string.

=item C<$COPYRIGHT> (string)

A multiline string which contains a summary of the copyright license.
S-SymObj is provided under the terms of the ISC license.

=item C<$MsgFH> (default: C<*STDERR>)

This is the file handle where all debug and verbose messages will be
written to.

=item C<$Debug> (0=off, 1=on, 2=verbose; default: 1)

If set to a value different to 0 then a lot of debug checks are
performed, and a rather slow object-constructor path is chosen (see
below).  If set to a value greater than 1 then message verbosity is
increased.  All messages go to L<"MsgFH">.

B<Note:> changing this value later on will neither affect the
object-constructor paths nor the per-object settings of all classes
and class-objects that have already been created/instantiated.

=item C<NONE>

Flag for L<"sym_create">, value 0.

=item C<DEBUG>

Bit-flag for L<"sym_create">, meaning to enable debug on a per-object
level.  Cannot be used to overwrite an enabled L<"Debug">, but will
be recognized otherwise.

=item C<VERBOSE>

Bit-flag for L<"sym_create">, meaning to enable verbosity on a per-object
level.  Cannot be used to overwrite an enabled L<"Debug">, but will
be recognized otherwise.

=item C<pack_exists(C<$1>=string=package/class)>

Check wether the class (package) $1 exists.

=item C<sym_dump($1=string OR object=symbol table target)>

Dump the symbol table entries of the package or object C<$1>.

=item C<obj_dump($1=$self)>

This is in fact a wrapper around C<Dumper::dump>.

=item C<sym_create($1=int=flags, $2=hash-ref=fields[, $3=code-ref/string])>

Create accessor methods/functions in the class package from within
which this function is called (best from within a C<BEGIN{}> block)
for all keys of C<$2>, after inspecting and adjusting them for access
modifiers, and do the "magic" S-SymObj symbol housekeeping, too.  C<$2>
may be the empty anonymous hash if the class does not introduce fields
on its own.  Note that a reference to C<$2> is internally stored as
L<"$_SymObj_FIELDS"> and used for the time being!  It should thus be
assumed that ownership of C<$2> is overtaken by S-SymObj.

C<$1> can be used to set per-class (package that is) flags, like
L<"DEBUG"> or L<"VERBOSE">.  Flags set like that will be inherited by
all subclasses (unless otherwise noted).  It is not possible to lower
the global L<"Debug"> state on this basis.

C<$3> is the optional per-object user constructor, which can be used
to perform additional setup of C<$self> as necessary.  These user
constructors take two arguments, C<$self>, the newly created object,
and C<$pkg>, the class name of the actual (sub)class that is
instantiated.  (Well, maybe partially created up to some point in
C<@ISA>.)  The user constructor doesn't have a return value.

If C<$3> is used, it must either be a code-reference or a string.  In
the latter case S-SymObj will try to locate a method of the given name
once the first object of the managed type is created, and set this to
be the user constructor.  If C<$3> is not used, S-SymObj will look for
a method named C<__ctor> once the first object of the managed type is
created.  B<Note> that the string and auto-search cases are not
thread-protected and may thus introduce races in multithreaded programs.
If in doubt, pass a code-reference.

SymObj generally "enforces" privacy (by definition) via an underscore
prefix: all keys of C<$2> are expected to start with an underscore,
but the public accessor methods will miss that (C<_data> becomes
C<data>).

The created accessor subs work as methods if the first argument that
is seen is a reference that seems to be a class instantiation (i.e.,
C<$self>, as in C<$self-E<gt>name()>) and as functions otherwise
(C<SomePack::name()>), in which case the provided package template
hash (C<$2>) is used!  (Note that no locking is performed in the latter
case, i.e., this should not be done in multithreaded programs.)  If
they act upon arrays or hashes they'll return references to the members
by default, but do return copies in C<wantarray> context instead.

If a key in C<$2> is prefixed with an AT or a PERCENT, as in C<'@_name'>
or C<'%_name'>, respectively, then the field in question is assumed
to be an array or hash, respectively.  By default S-SymObj uses the
value to figure out which kind of accessor has to be used, but for
that the value must be set to a value different than C<undef>, which
is sometimes not desirable, e.g., when a field should be lazy allocated,
only if it is really used.  Note that the generic accessors will
automatically instantiate an empty array/hash as necessary in these
cases.

After the (optional) C<@> or C<%> type modifier, one may use (also
optionally) C<?> or C<!>, mutually exclusive, as an access modifier.
If a question mark is seen, as in C<'?_name'>, then this means that
no accessor subs will be created for C<name>.  Just likewise for
exclamation mark, as in C<'!_name'>, there will only be a readonly
accessor sub available.  Write access will be actively rejected in
this case.

Whatever type and/or access modifier(s) was/were present, they will
be stripped from the field name, just like a following underscore will,
i.e., a field C<'@!_name'> will actually end up as C<_name> in the
class-static hash, with an accessor sub named C<name>.

In addition to those accessor subs there will I<always> be private
accessor subs be created which use the public name prefixed with two
underscores, as in C<$self-E<gt>__name()>.  These subs do nothing
except returning a reference to the field in question.  They're ment
to be used instead of direct access of members in some contexts, i.e.,
for encapsulation purposes.  Note that they do I<not> automatically
instantiate lazy allocated fields.

Note that, if any of the superclasses of C<$1>, as detected through
its C<@ISA> array, provides fields which have identical names to what
is provided in C<$2>, this situation is treated as an overwrite request,
and it is verified that the value type matches.  Unfortunately the
subclass will create accessor subs on its own, because users need to
be able to adjust the class-static data.  And, also unfortunately,
different access policies won't be detected.

=back

=head1 INJECTED SYMBOLS

For completeness, here is a list of all the symbols that S-SymObj
creates internally for each managed package.

=over

=item C<$_SymObj_PACKAGE>

The C<__PACKAGE__>.

=item C<$_SymObj_ISA>

A copy of the class's C<@ISA>, including the class itself.  This indeed
is the entire unfolded class tree, unfolded in down-top, left-right
(a.k.a construction) order.

=item C<$_SymObj_ALL_CTOR_ARGS>

A hash that includes all arguments that the constructor is allowed to
take.  (Won't cover classes that are not managed by S-SymObj.)

=item C<$_SymObj_CTOR_OVERRIDES>

A list of all fields that this package overrides from superclasses.
(Won't cover classes that are not managed by S-SymObj.)

=item C<$_SymObj_FIELDS>

The reference to the field hash, as given to L<"sym_create"> (modified
to not include field-access modifier characters).

=item C<$_SymObj_FLAGS>

Some flags. :)

=item C<$_SymObj_USR_CTOR>

An optional field that holds a reference to the users constructor (once
resolved).

=item C<new()>

The auto-generated public class constructor.

=item C<_SymObj_ArraySet()>

Shared array handler, only if needed.

=item C<_SymObj_HashSet()>

Shared hash handler, only if needed.

=back

=head1 FUTURE DIRECTIONS

Optional (flag driven) thread safety for static data access.
Thread safety for resolving the user-constructor (maybe).

Maybe add support for class members, but the problem here is of course
the default-argument nature of S-SymObj; we could however require
initialization of the member with a C<package, callback> tuple to (1)
test references of given objects (via perl(1) UNIVERSAL, then) and
initialize default objects if none has been given by user.

Finally: realize that perl(1) ships with struct and class and similar
things which head in the very same direction as S-SymObj.  I want to
point out that i wrote this package the hard way.  It is me.

=head1 LICENSE

Copyright (c) 2010 - 2012 Steffen "Daode" Nurpmeso.
All rights reserved under the terms of the ISC license.

=cut
# }}}

# vim:set fenc=utf-8 syntax=perl ts=8 sts=3 sw=3 et tw=79:
