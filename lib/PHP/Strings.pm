package PHP::Strings;
use strict;
use warnings FATAL => 'all';
our $VERSION = '0.24';

=head1 NAME

PHP::Strings - Implement some of PHP's string functions.

=head1 SYNOPSIS

# XXX

=head1 DESCRIPTION

PHP has many functions. This is one of the main problems with PHP.

People do, however, get used to said functions and when they come to a
better designed language they get lost because they have to implement
some of these somewhat vapid functions themselves.

So I wrote C<PHP::Strings>. It implements most of the strings functions
of PHP. Those it doesn't implement it describes how to do in native
Perl.

Any function that would be silly to implement has not been and has been
marked as such in this documentation. They will still be exportable, but
if you attempt to use said function you will get an error telling you to
read these docs.

=head1 ERROR HANDLING

All arguments are checked using L<Params::Validate>. Bad arguments will
cause an error to be thrown. If you wish to catch it, use C<eval>.

=cut

use base qw( Exporter );
use Carp qw( croak );
use vars qw( %EXPORT_TAGS @EXPORT @EXPORT_OK @badeggs );
use Params::Validate qw( :all );
use Scalar::Util qw( looks_like_number );
use List::Util qw( shuffle );
use HTML::TokeParser;

use constant STRING => {
    type => SCALAR,
};
use constant INTEGER => {
    type => SCALAR,
    regex => qr{^\d+$}
};
use constant NUMBER => {
    type => SCALAR,
    callbacks => {
        'is a number' => sub {
            defined $_[0] and looks_like_number $_[0]
        }
    },
};

sub NUMBER_RANGE ($$) {
    my ($min, $max) = @_;
    return {
        %{+INTEGER},
        callbacks => {
            "Number between $min and $max" => sub {
                $_[0] =~ /^\d+$/ and $min <= $_[0] and $_[0] <= $max
            }
        }
    };
}

=head1 EXPORTS

By default, nothing is exported.

Each function and constant can be exported by explicit name.

    use PHP::Strings qw( str_pad addcslashes );

To get a function and its associated constants as well, prefix them with a colon:

    use PHP::Strings qw( :str_pad );
    # This grabs str_pad, STR_PAD_LEFT, STR_PAD_BOTH, STR_PAD_RIGHT.

To export everything:

    use PHP::Strings qw( :all );

For more information on what you can add there, consult
L<Exporter/"Specialised Import Lists">.

=cut

@EXPORT_OK = map { @$_ } values %EXPORT_TAGS;

=head1 FUNCTIONS

=head2 addcslashes

=cut

BEGIN { $EXPORT_TAGS{addcslashes} = [ qw(
    addcslashes
    ) ] }

sub addcslashes
{
    my ($str, $charlist) = validate_pos( @_,
        STRING, STRING,
    );

    my @patterns = split /(.\.\..)/, $charlist;
    for (@patterns) {
        if ( m/ \A (.)\.\.(.) \z /x ) {
            if ( ord $1 > ord $2 ) {
                $_ = "\Q$1$2.";
            } else {
                $_ = "\Q$1\E-\Q$2\E";
            }
        } else {
            $_ = "\Q$_";
        }
    }
    my $tr = join '', @patterns;
    $str =~ s/([$tr])/\\$1/g;

    return $str;
}

=head2 addslashes

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Returns a string with backslashes before characters that need to be
quoted in SQL queries. You should never need this function. I mean,
never.

L<DBI>, the standard method of accessing databases with perl, does all
this for you. It provides by a C<quote> method to escape anything, and
it provides placeholders and bind values so you don't even have to worry
about escaping. In PHP, PEAR DB also provides this facility.

L<DBI> is also aware that some databases don't escape in this method,
such as mssql which uses doubled characters to escape (like some
versions of BASIC). This function doesn't.

The less said about PHP's C<magic_quotes> "feature", the better.

=cut

BEGIN { push @badeggs, "addslashes" }

=head2 bin2hex

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

This is trivially implemented using L<pack|perlfunc/"pack">.

    my $hex = unpack "H*", $data;

=cut

BEGIN { push @badeggs, "bin2hex" }

=head2 chop

PHP's C<chop> function is an alias to its L<"rtrim"> function.

Perl has a builtin named L<chop|perlfunc/"chop">. Thus we do
not support the use of C<chop> as an alias to L<"rtrim">.

=head2 chr

PHP's and Perl's L<chr|perlfunc/"chr"> functions operate sufficiently
identically.

Note that PHP's claims an ASCII value as input. Perl assumes Unicode.
But ensure you see L<the documentation|perlfunc/"chr"> for a precise
definition.

Note that it returns B<one character>, which in some string
encodings may not necessarily be B<one byte>.

=head2 chunk_split

Returns the given string, split into smaller chunks.

    my $split = chunk_split( $body [, $chunklen [, $end ] ] );

Where C<$body> is the data to split, C<$chunklen> is the optional length
of data between each split (default 76), and C<$end> is what to insert
both between each split (default C<"\r\n">) and on the end.

Also trivially implemented as a regular expression:

    $body =~ s/(.{$chunklen})/$1$end/sg;
    $body .= $end;

=cut

BEGIN { $EXPORT_TAGS{chunk_split} = [ qw(
    chunk_split
    ) ] }

sub chunk_split
{
    my ( $body, $chunklen, $end ) = validate_pos( @_,
        STRING,
        { %{+INTEGER}, optional => 1, default => 76 },
        { %{+STRING}, optional => 1, default => "\r\n" },
    );

    $body =~ s/(.{$chunklen})/$1$end/sg;
    $body .= $end;

    return $body;
}

=head2 convert_cyr_string

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Perl has the L<Encode> module to convert between character encodings.

=cut

BEGIN { push @badeggs, "convert_cyr_string" }

=head2 count_chars

A somewhat daft function that returns counts of characters in a string.

It's daft because it assumes characters have values in the range 0-255.
This is patently false in today's world of Unicode. In fact, the PHP
documentation for this function happily talks about characters in one
part and bytes in another, not realising the distinction.

So, I've implemented this function as if it were called C<count_bytes>.
It will count raw bytes, not characters.

Takes two arguments: the byte sequence to analyse and a 'mode' flag that
indicates what sort of return value to return. The default mode is C<0>.

   Mode  Return value
   ----  ------------
    0    Return hash of byte values and frequencies.
    1    As for 0, but hash does not contain bytes with frequency of 0.
    2    As for 0, but hash only contains bytes with frequency of 0.
    3    Return string composed of used byte-values.
    4    Return string composed of unused byte-values.

   my %freq = count_chars( $string, 1 );

=cut

BEGIN { $EXPORT_TAGS{count_chars} = [ qw(
    count_chars
    ) ] }

sub count_chars
{
    my ( $input, $mode ) = validate_pos( @_,
        STRING,
        {
            %{+NUMBER_RANGE( 0, 4 )},
            optional => 1,
            default => 0
        },
    );

    if ( $mode < 3 ) # Frequency
    {
        use bytes;
        my %freq;
        @freq{0..255} = (0) x 256 if $mode != 1;
        $freq{ord $_}++ for split //, $input;
        if ( $mode == 2 ) {
            $freq{$_} and delete $freq{$_} for keys %freq;
        }
        return %freq;
    }
    else
    {
        my %freq = count_chars( $input, $mode-2 );
        return join '', map chr, sort keys %freq;
    }

    croak "Reached a line we should not have.";
}

=head2 crc32

XXX

=head2 crypt

PHP's crypt is the same as Perl's. Thus there's no need for
C<PHP::String> to provide an implementation.

The C<CRYPT_*> constants are not provided.

=head2 echo

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"print">.

=cut

BEGIN { push @badeggs, "echo" }

=head2 explode

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Use the C<\Q> regex metachar and L<split|perlfunc/"split">.

   my @pieces = split /\Q$separator/, $string, $limit;

See L<perlfunc/"split"> for more details.

Note that C<split //> will split between every character, rather than
returning false. Note also that C<split "..."> is the same as
C<split /.../> which means to split everywhere three characters are
matched. The first argument to C<split> is always a regex.

=cut

BEGIN { push @badeggs, "explode" }

=head2 fprintf

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Perl's L<printf|perlfunc/"printf"> can be told to which file handle to
print.

    printf FILEHANDLE $format, @args;

See L<perlfunc/"printf"> and L<perlfunc/"print"> for details.

=cut

BEGIN { push @badeggs, "fprintf" }

=head2 get_html_translation_table

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Use the L<HTML::Entities> module to escape and unescape characters.

=cut

BEGIN { push @badeggs, "get_html_translation_table" }

=head2 hebrev

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Use the L<Encode> module to convert between character encodings.

=cut

BEGIN { push @badeggs, "hebrev" }

=head2 hebrevc

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Use the L<Encode> module to convert between character encodings.

=cut

BEGIN { push @badeggs, "hebrevc" }

=head2 html_entity_decode

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Use the L<HTML::Entities> module to decode character entities.

=cut

BEGIN { push @badeggs, "html_entity_decode" }

=head2 htmlentities

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Use the L<HTML::Entities> module to encode characters as HTML entities.

=cut

BEGIN { push @badeggs, "htmlentities" }

=head2 htmlspecialchars

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Use the L<HTML::Entities> module to encode characters as HTML entities.

=cut

BEGIN { push @badeggs, "htmlspecialchars" }

=head2 implode

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"join">. Note that join cannot accept its arguments in
either order because that's just not how Perl arrays and lists work.
Note also that the joining sequence is not optional.

=cut

BEGIN { push @badeggs, "implode" }

=head2 join

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

PHP's C<join> is an alias for C<implode>. See L<"implode">.

=cut

=head2 levenshtein

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

I have no idea why PHP has this function.

See L<Text::Levenshtein>, L<Text::LevenshteinXS>, L<String::Approx>,
L<Text::PHraseDistance> and probably any number of other modules on
CPAN.

=cut

BEGIN { push @badeggs, "levenshtein" }

=head2 localeconv

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<perllocale>.

=cut

BEGIN { push @badeggs, "localeconv" }

=head2 ltrim

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

As per L<perlfaq>:

   $string =~ s/^\s+//;

A basic glance through L<perlretut> or L<perlreref> should give you an
idea on how to change what characters get trimmed.

=cut

BEGIN { push @badeggs, "ltrim" }

=head2 md5_file

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

The L<Digest::MD5> module provides sufficient support.

    use Digest::MD5;

    sub md5_file
    {
        my $filename = shift;
        my $ctx = Digest::MD5->new;
        open my $fh, '<', $filename or die $!;
        binmode( $fh );
        $ctx->addfile( $fh )->digest; # or hexdigest, or b64digest
    }

Despite providing that possible implementation just above, I've chosen to
not include it as an export due to the amount of flexibility of
L<Digest::MD5> and the number of ways you may want to get your file
handle. After all, you may want to use L<Digest::SHA1>, or
L<Digest::MD4> or some other digest mechanism.

Again, I wonder why PHP has the function as they so arbitrarily
hobble it.

=cut

BEGIN { push @badeggs, "md5_file" }

=head2 md5

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<Digest::MD5> which provides a number of functions for computing
MD5 hashes from various sources and to various formats.

Note: the user notes for this function at http://www.php.net/md5 are
among the most unintentionally funny and misinformed I've read.

=cut

BEGIN { push @badeggs, "md5" }

=head2 metaphone

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

L<Text::Metaphone> and L<Text::DoubleMetaphone> and
L<Text::TransMetaphone> all provide metaphonic calculations.

=cut

BEGIN { push @badeggs, "metaphone" }

=head2 money_format

=cut

BEGIN { $EXPORT_TAGS{money_format} = [ qw(
    money_format
    ) ] }

sub money_format
{
    my ( $format, @amounts ) = validate_with(
        params => \@_,
        allow_extra => 1,
        spec => [
            {
                type => SCALAR,
            },
            NUMBER,
        ]
    );

    my $rv = _strfmon( $format, @amounts );

    return $rv;
}

=head2 nl_langinfo

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

L<I18N::Langinfo> has a C<langinfo> command that corresponds to PHP's
C<nl_langinfo> function.

=cut

BEGIN { push @badeggs, "nl_langinfo" }

=head2 nl2br

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

This is trivially implemented as:

    s,$,<br />,mg;

=cut

BEGIN { push @badeggs, "nl2br" }

=head2 number_format

=cut

sub number_format
{
    my ( $number, $decimals, $dec, $thousands ) = validate_pos( @_,
        NUMBER,
        { %{+NUMBER}, optional => 1 },
        { %{+STRING}, optional => 1, default => '.' },
        { %{+STRING}, optional => 1, default => ',' },
    );

    my $format = $decimals ? "%.${decimals}f" : "%d";

    my $formatted = 'XXX';

    return $formatted;
}

=head2 ord

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"ord">. Note that Perl returns Unicode value, not ASCII.

=cut

BEGIN { push @badeggs, "ord" }

=head2 parse_str

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See instead the L<CGI> and L<URI> modules which handles that sort of
thing.

=cut

BEGIN { push @badeggs, "parse_str" }

=head2 print

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"print">.

=cut

#BEGIN { push @badeggs, "print" }

=head2 printf

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"printf">.

=cut

#BEGIN { push @badeggs, "printf" }

=head2 quoted_printable_decode

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

L<MIME::QuotedPrint> provides functions for encoding and decoding
quoted-printable strings.

=cut

BEGIN { push @badeggs, "quoted_printable_decode" }

=head2 quotemeta

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"quotemeta">.

=cut

BEGIN { push @badeggs, "quotemeta" }

=head2 rtrim

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Another trivial regular expression:

   $string =~ s/\s+$//;

See the notes on L<"ltrim">.

=cut

BEGIN { push @badeggs, "rtrim" }

=head2 setlocale

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

C<setlocale> is provided by the L<POSIX> module.

=cut

#BEGIN { push @badeggs, "setlocale" }

=head2 sha1_file

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<"md5_file">

=cut

BEGIN { push @badeggs, "sha1_file" }

=head2 sha1

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<"md5">, mentally substituting L<Digest::SHA1> for L<Digest::MD5>,
although the user notes are not as funny.

=cut

BEGIN { push @badeggs, "sha1" }

=head2 similar_text

XXX

=cut

=head2 soundex

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<Text::Soundex>, which also happens to be a core module.

=cut

BEGIN { push @badeggs, "soundex" }

=head2 sprintf

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"sprintf">.

=cut

BEGIN { push @badeggs, "sprintf" }

=head2 sscanf

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

This is a godawful function. You should be using regular expressions
instead. See L<perlretut> and L<perlre>.

=cut

BEGIN { push @badeggs, "sscanf" }

=head2 str_ireplace

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Use the C<s///> operator instead. See L<perlop> and L<perlre> for
details.

=cut

BEGIN { push @badeggs, "str_ireplace" }

=head2 str_pad

=cut

BEGIN { $EXPORT_TAGS{str_pad} = [ qw(
    str_pad STR_PAD_RIGHT STR_PAD_LEFT STR_PAD_BOTH
    ) ] }

use constant STR_PAD_RIGHT => 1;
use constant STR_PAD_LEFT  => 2;
use constant STR_PAD_BOTH  => 3;

sub str_pad
{
    my ( $input, $length, $pad, $options ) = validate_pos( @_,
        STRING,
        INTEGER,
        { %{+STRING}, optional => 1, default => ' ' },
        { %{+INTEGER}, optional => 1, default => STR_PAD_RIGHT },
    );

    return $input if $length < length $input;

    # Work out where to place our string.
    my $start = 0;
    my $diff = $length - length $input;
    my $rv;

    if ( $options == STR_PAD_RIGHT )
    {
        my $padding = substr( $pad x $diff, 0, $diff );
        $rv = $input . $padding;
    }
    elsif ( $options == STR_PAD_LEFT )
    {
        my $padding = substr( $pad x $diff, 0, $diff );
        $rv = $padding . $input;
    }
    elsif ($options == STR_PAD_BOTH )
    {
        $rv = substr( $pad x $length, 0, $length );
        substr( $rv, $diff / 2, length $input ) = $input;
    }
    else
    {
        croak "Invalid 4th argument to str_pad";
    }

    $rv;
}

=head2 str_repeat

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Instead, use the C<x> operator. See L<perlop> for details.

    my $by_ten = "-=" x 10;

=cut

BEGIN { push @badeggs, "str_repeat" }

=head2 str_replace

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See the C<s///> operator. L<perlop> and L<perlre> have details.

=cut

BEGIN { push @badeggs, "str_replace" }

=head2 str_rot13

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

This is rather trivially implemented as:

    $message =~ tr/A-Za-z/N-ZA-Mn-za-m/

(As per "Programming Perl", 3rd edition, section 5.2.4.)

=cut

BEGIN { push @badeggs, "str_rot13" }

=head2 str_shuffle

Implemented, against my better judgement. It's trivial, like so many of
the others.

=cut

BEGIN { $EXPORT_TAGS{str_shuffle} = [qw( str_shuffle )] }

sub str_shuffle
{
    my ( $string ) = validate_pos( @_, STRING );

    return join '', shuffle split //, $string;
}

=head2 str_split

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"split"> for details.

    my @bits = split /(.{,$len})/, $string;

=cut

BEGIN { push @badeggs, "str_split" }

=head2 str_word_count



=cut

BEGIN { $EXPORT_TAGS{str_word_count} = [qw( str_word_count )] }

sub str_word_count
{
    my ( $string, $format ) = validate_pos( @_,
        STRING,
        {
            %{+NUMBER_RANGE( 0, 1 )},
            default => 1,
        }
    );

    if ( $format == 1 ) {
        my @words = $string =~ m/(\S+)/g;
        return @words;
    } else {
        my %words;
        while ( $string =~ m/(\S+)/g )
        {
            $words{ $-[1] } = $1;
        }
        return %words;
    }
}

=head2 strcasecmp

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Equivalent to:

    lc($a) cmp lc($b)

=cut

BEGIN { push @badeggs, "strcasecmp" }

=head2 strchr

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

See L<"strstr">

=cut

BEGIN { push @badeggs, "strchr" }

=head2 strcmp

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Equivalent to:

    $a cmp $b

=cut

BEGIN { push @badeggs, "strcmp" }

=head2 strcoll

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Equivalent to:

    use locale;

    $a cmp $b

=cut

BEGIN { push @badeggs, "strcoll" }

=head2 strcspn

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Trivially equivalent to:

    my $cspn;
    $cspn = $-[0]-1 if $string =~ m/[chars]/;

=cut

BEGIN { push @badeggs, "strcspn" }

=head2 strip_tags

B<THIS FUNCTION WILL NOT BE IMPLEMENTED>.

Gosh. That was hard.

=cut

BEGIN { $EXPORT_TAGS{strip_tags} = [qw( strp_tags )] }

sub strip_tags
{
    my ( $html, $exclusion ) = validate_pos( @_,
        STRING,
        {
            %{+STRING},
            default => '',
        }
    );

    my %ignore = map { $_, 1 } ( $exclusion =~ /<(\w+)>/g );
    my @nexts = map { ( $_, "/$_" ) } keys %ignore;
    my $p = HTML::TokeParser->new( \$html );

    my $rv;
    while ( my $text = $p->get_text( @nexts ) )
    {
        $rv .= $text;
    }
    $rv .= $p->get_tag->[1];

    return $rv;
}


# ========================================================================

=head1 BAD EGGS

All functions that I think are worthless are still exportable, with the
exception of any that would clash with a Perl builtin function.

If you try to actually use said function, a big fat error will result.

=cut

BEGIN {
    no strict 'refs';
    for my $fn ( @badeggs )
    {
        $EXPORT_TAGS{$fn} = [ $fn ];
        *$fn = sub {
            croak <<"EOF";
Function `$fn' not implemented.

Consult `perldoc PHP::Strings` for the reason why.
EOF
        }
    }
}

=begin _private

=head1 XS

Some functions are implemented in C. This is done either out of ease of
programming (L<"money_format"> is just a façade for strfmon(3)), or
because C is sometimes just the right tool (mainly in dealing with
individual character manipulation of strings).

=cut

require XSLoader;
XSLoader::load('PHP::Strings', $VERSION);

=end _private

=cut

1;

__END__

=head1 FOR THOSE WHO HAVE READ THIS FAR

Yes, this module is mostly a joke. I wrote a lot of it after
being asked for the hundredth time: What's the equivalent to
PHP's X in Perl?

That said, although it's a joke, I'm happy to receive
amendments, additions and such. It's incomplete at present,
and I would like to see it complete at some point.

In particular, the test suite needs a lot of work. (If you
feel like it. Hint Hint.)

=head1 BUGS, REQUESTS, COMMENTS

Log them via the CPAN RT system via the web or email:

    http://rt.cpan.org/NoAuth/ReportBug.html?Queue=PHP-Strings
    ( shorter URL: http://xrl.us/4at )

    bug-php-strings@rt.cpan.org

This makes it much easier for me to track things and thus means
your problem is less likely to be neglected.

=head1 LICENCE AND COPYRIGHT

PHP::Strings is copyright E<copy> Iain Truskett, 2003. All rights
reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.000 or,
at your option, any later version of Perl 5 you may have available.

The full text of the licences can be found in the F<Artistic> and
F<COPYING> files included with this module, or in L<perlartistic> and
L<perlgpl> as supplied with Perl 5.8.1 and later.

=head1 AUTHOR

Iain Truskett <spoon@cpan.org>

=head1 SEE ALSO

L<perl>, L<php>.

=cut
