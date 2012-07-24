#@ (S-)Sym(bolic)Obj(ect) - easy creation of classes and objects thereof.
package SymObj;
require 5.008;
$VERSION = '0.6.0a';
$COPYRIGHT =<<_EOT;
Copyright (c) 2010 - 2012 Steffen Daode Nurpmeso <sdaoden\@users.sf.net>.
All rights reserved.
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

# Yeah, we fool around with that by definition, so this
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

sub _UUID { return 'S-SymObj::1C8288D6-9EDA-4ECD-927F-2144B94186AD'; }

# TODO pack_ series incomplete (available in eval?? require??)
sub pack_exists {
   my ($pkg) = @_;
   return defined %{"${pkg}::"};
}

sub sym_create {
   my ($pkg, $tfields) = @_;
   print STDERR "SymObj::sym_create(): $pkg\n" if $SymObj::Verbose;

   # Minimize code-blow - offer some basic SymObj symtable entries.
   # Even that is overkill unless the %FIELDS really require that though.
   # Since we use these also for management tasks, then, do inject regardless
   # of wether the symbols are public or not.
   my $i = 0;
   foreach my $datum (keys %$tfields) {
      if (ref $tfields->{$datum} eq 'ARRAY') {
         $i |= 1;
         last if ($i & 3) == 3;
      } elsif (ref $tfields->{$datum} eq 'HASH') {
         $i |= 2;
         last if ($i & 3) == 3;
      }
   }
   if ($i & 1) {
      print STDERR "SymObj::sym_create(): $pkg: adding shared array handler\n"
         if $SymObj::Verbose;
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

         return $dref;
      };
   }
   if ($i & 2) {
      print STDERR "SymObj::sym_create(): $pkg: adding shared hash handler\n"
         if $SymObj::Verbose;
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
                  if (@$arg != 0 && $SymObj::Debug);
            } else {
               $k = $arg;
            }
         }
         print STDERR "! ${pkg}::${pub}(): '$k' key without a value\n"
            if (defined $k && $SymObj::Debug);

         return $dref;
      };
   }

   # For (superior debug ctor) argument checking, create a hash of
   # public symbols (inherit those of parents first ...)
   my %socargs;
   *{"${pkg}::_SymObj_AllCTorArgs"} = \%socargs;

   if (defined *{"${pkg}::ISA"}) {
      foreach my $super (@{*{"${pkg}::ISA"}}) {
         my $socr = *{"${super}::_SymObj_AllCTorArgs"};
         unless (defined $socr) {
            print STDERR "! ${pkg}: \@ISA entry '$super' is not a SymObj ",
               "managed class!\n";
            next;
         }
         $socargs{$_} = $_ foreach (keys %$socr);
      }
   }

   # We need a pointer to the fields for the constructor - overtake ownership
   *{"${pkg}::_SymObj_Fields"} = $tfields;

   # Create accessor symtable entries
   my @dellist;
   foreach my $datum (keys %$tfields) {
      my ($xdatum, $pub) = ($datum, $datum);
      my ($isex) = (0);

      # Don't create accessor subs?
      if ($pub =~ /^\?/o) {
         $isex = 1;
         $xdatum =
         $pub = substr($pub, 1);
         push @dellist, [$datum, $xdatum];
      }

      if ($pub =~ /^_/) {
         $pub = substr($pub, 1);
      } elsif ($SymObj::Verbose) {
         print STDERR "\tSymbol '$pub': does NOT start with underscore _!\n";
      }
      $socargs{$pub} = $pub;

      if ($isex) {
         print STDERR "\tSymbol '$pub' excluded\n" if $SymObj::Verbose;
         next;
      }

      if (ref $tfields->{$datum} eq 'ARRAY') {
         print STDERR "\tsub $pub: array-based\n" if $SymObj::Verbose;
         *{"${pkg}::$pub"} = sub {
            my $self = $_[0];
            if (ref $self) { shift; }
            else           { $self = $tfields; }
            $self = ((@_ == 0) ? $self->{$xdatum}
                   : "${pkg}::_SymObj_ArraySet"->($self, $pub, $xdatum, @_));
            return wantarray ? @$self : $self;
         };
      } elsif (ref $tfields->{$datum} eq 'HASH') {
         print STDERR "\tsub $pub: hash-based\n" if $SymObj::Verbose;
         *{"${pkg}::$pub"} = sub {
            my $self = $_[0];
            if (ref $self) { shift; }
            else           { $self = $tfields; }
            $self = ((@_ == 0) ? $self->{$xdatum}
                   : "${pkg}::_SymObj_HashSet"->($self, $pub, $xdatum, @_));
            return wantarray ? %$self : $self;
         };
      } else {
         # Scalar (or "typeless")
         *{"${pkg}::$pub"} = sub {
            my $self = $_[0];
            if (ref $self) { shift; }
            else           { $self = $tfields; }
            $self->{$xdatum} = shift if @_;
            return $self->{$xdatum};
         };
      }
   }

   # Finally refcopy all those fields which end up without a special prefix
   foreach my $l (@dellist) {
      $tfields->{$l->[1]} = $tfields->{$l->[0]};
      delete $tfields->{$l->[0]};
   }
}

sub sym_dump {
   my ($pkg) = @_;
   print STDERR "SymObj::sym_dump($pkg):\n\t";
   if (my $i = ref $pkg) { $pkg = $i; }
   foreach (keys %{*{"${pkg}::"}}) { print STDERR "$_ "; }
   print STDERR "\n";
}

sub obj_ctor {
   my ($pkg, $class, $argaref) = @_;
   my $over = (@_ > 3) ? $_[3] : undef;
   print STDERR "SymObj::obj_ctor <> new: $pkg called as $class\n"
      if $SymObj::Verbose;

   my ($self, $argc, $init_chain) = (undef, scalar @$argaref, 0);

   # Since all classes use obj_ctor in their new, we would perform argument
   # checking over and over again; so use a savage and hacky, but
   # multithread-safe way to perform argument checking only in the ctor of
   # the real (actual sub-) class
   if ($SymObj::Debug && $argc > 0 &&
         defined $argaref->[0] && $argaref->[0] eq _UUID) {
      $init_chain = 1;
   }

   # Inheritance handling
   if (defined(my $isa = *{"${pkg}::ISA"})) {
jOVER_OUTER:
      # Append overrides
      foreach my $k (keys %$over) {
         for (my $i = $init_chain; $i < $argc; $i += 2) {
            next jOVER_OUTER if $k eq $$argaref[$i];
         }
         push @$argaref, $k;
         push @$argaref, $over->{$k};
      }

      # Walk the new() chain,
      # but disallow arg-checking for superclasses
      unshift(@$argaref, _UUID) if ($SymObj::Debug && !$init_chain);

      foreach (@$isa) {
         my $sym = "${_}::new";
         unless (defined *$sym) {
            next unless $SymObj::Debug;
            print STDERR "${pkg}: $class->new(): no such package: $_!\n"
               and next unless defined *{"${_}::"};
            print STDERR "${pkg}: $class->new(): $_: misses a new() sub!\n";
            next;
         }
         $sym = &$sym($class, @$argaref);

         # (MI restriction applies here: if $self is yet
         # a {} the other tree can only be joined in and
         # thus looses it's hash-pointer)
         $self = $sym and next unless defined $self;
         while (my ($k, $v) = each %$sym) { $self->{$k} = $v; }
      }

      shift @$argaref if ($SymObj::Debug && !$init_chain);
   }

   # SELF
   $self = {} unless defined $self;
   $self = bless $self, $class;

   # Normal arguments; can and should we perform argument checking?
   $over = undef;
   if ($SymObj::Debug) {
      if ($init_chain) {
         shift @$argaref;
      } else {
         $over = *{"${pkg}::_SymObj_AllCTorArgs"};
         if (($argc & 1) != 0) {
            --$argc;
            print STDERR "${pkg}: $class->new(): odd argument discarded!\n"
         }
      }
   }

   # We need the per-class template member fields for comparison plus
   my $tfields = *{"${pkg}::_SymObj_Fields"};

   # Use generic internal accessor (public ones may be excluded..)
   for (my $i = -1; $argc > 1; $argc -= 2) {
      my $k = $argaref->[++$i];
      my $pk = '_' . $k;
      my $v = $argaref->[++$i];

      my $tv = $tfields->{$pk};
      unless (exists $tfields->{$pk}) {
         next unless defined $over;
         next if exists $over->{$k};
         print STDERR "${pkg}: $class->new(): unknown argument: '$k'\n";
      } elsif (ref $tv eq 'ARRAY') {
         $self->_SymObj_ArraySet('new()', $pk, $v);
      } elsif (ref $tv eq 'HASH') {
         unless (ref $v eq 'ARRAY' || ref $v eq 'HASH') {
            print STDERR "${pkg}: $class->new(): ",
               "'$k' requires ARRAY or HASH argument\n";
            next;
         }
         $self->_SymObj_HashSet('new()', $pk, $v);
      } else {
         $self->{$pk} = $v;
      }
   }

   # Finally: fill in yet unset members of $self via the per-class template
   # By default anon-hashes and -arrays get reference-copied;
   # we however *do* need a detached (deep) copy!
   while (my ($k, $v) = each %$tfields) {
      next if exists $self->{$k};
      unless (ref $v) {
         $self->{$k} = $v;
      } elsif (ref $v eq 'ARRAY') {
         my @a;
         push(@a, $_) foreach (@$v);
         $self->{$k} = \@a;
      } elsif (ref $v eq 'HASH') {
         my (%h, $hk, $hv);
         while (($hk, $hv) = each %$v) { $h{$hk} = $hv; }
         $self->{$k} = \%h;
      } elsif ($SymObj::Debug) {
         print STDERR "${pkg}: $class->new(): value of '$k' has an ",
            "unsupported type!\n";
      }
   }

   return $self;
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
      $SymObj::Verbose = 1;
      #$SymObj::Debug = 0;
   }

   {package X_Super;
      BEGIN {
         SymObj::sym_create(__PACKAGE__, {
            _name => '', _array => [qw(av1 av2)],
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

   # Adjust the per-class template
   undef %{X_Super::hash()};
   X_Super::hash(newhk1=>'newhv1', newhk2=>'newhv2');
   $sp = SomePack->new(name => 'SymObj is really easy');
   SymObj::obj_dump($sp);


=head2 Package-Symbols

=over

=item C<$SymObj::VERSION> (string, i.e., '0.6.0a')

A version string.

=item C<$SymObj::COPYRIGHT> (string)

Copyright multiline string, formatted for pretty printing.

=item C<$SymObj::Debug> (boolean, i.e., 0 or 1, default 1)

Indicates wether some checks etc. shall be performed or not.
By default enabled.
Messages go to STDERR.

=item C<$SymObj::Verbose> (boolean, i.e., 0 or 1, default 0)

If enabled some more informational etc. messages go to STDERR.
By default disabled.

=item C<pack_exists(C<$1>=string=package/class)>

Check wether the class (package) $1 exists.

=item C<sym_create($1=string=package, $2=hash-ref=fields)>

Create accessor methods/functions in the C<__PACKAGE__> C<$1>
for all keys of C<$2>
I<and> do some more magic symbol table housekeeping to make SymObj work.
Due to the additional magic this must be called even if no fields exist,
yet even if C<$2> is the empty anonymous C<{}> hash (as shown above).

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

If keys in C<$2> are prefixed with a question mark, as in C<'?_name'>,
then this means that no accessor sub will be created for C<name>.  The
mark will be stripped internally, i.e., the member is C<_name>, as
expected, and that's also the way it is handled otherwise.

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

=item C<obj_dump($1=$self)>

Dumper::dump.

=back

=cut
# vim:set fenc=utf-8 filetype=perl syntax=perl ts=3 sts=3 sw=3 et tw=79:
