package PHP::Strings;
# vim: ft=perl
use strict;
use warnings FATAL => 'all';
our $VERSION = '0.25';

=head1 NAME

PHP::Strings - Implement some of PHP's string functions.

=head1 SYNOPSIS

   use PHP::Strings;

   my $slashed = addcslashes( $not_escaped, $charlist );



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

=head1 RELATED READING

=over 4

=item *

"PHP in Contrast to Perl"
http://tnx.nl/php.txt

=item *

"Experiences of Using PHP in Large Websites" by Aaron Crane, 2002
http://www.ukuug.org/events/linux2002/papers/html/php/

=item *

"PHP Annoyances" by Neil de Carteret, 2002
http://n3dst4.com/articles/phpannoyances/

=item *

"I hate PHP" by Keith Devens, 2003
http://keithdevens.com/weblog/archive/2003/Aug/13/HATE-PHP

=item *

"PHP: A love and hate relationship" by Ivan Ristic, 2002
http://www.webkreator.com/php/community/php-love-and-hate.html

=item *

"PHP Sucks"
http://czth.net/pH/PHPSucks

=item *

Nathan Torkington's "list of PHP's shortcomings"
http://nntp.x.perl.org/group/perl.advocacy/1458

=back

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

sub death
{
    local $_ = shift;
    s/^=.*$//gm;
    s/^\n+//g;
    s/\n+$//g;
    croak "\n$_\n\n";
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



=head1 addcslashes

http://www.php.net/addcslashes


Returns a string with backslashes before characters that are listed
in C<$charlist>.


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



=head1 addslashes

http://www.php.net/addslashes


=cut

sub addslashes {
    death(<<'EODEATH');

=pod

B<PHP::Strings::addslashes WILL NOT BE IMPLEMENTED>.

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

EODEATH
}



BEGIN { push @badeggs, "addslashes" };


=head1 bin2hex

http://www.php.net/bin2hex


=cut

sub bin2hex {
    death(<<'EODEATH');

=pod

B<PHP::Strings::bin2hex WILL NOT BE IMPLEMENTED>.

This is trivially implemented using L<pack|perlfunc/"pack">.

    my $hex = unpack "H*", $data;


=cut

EODEATH
}



BEGIN { push @badeggs, "bin2hex" };


=head1 chop

http://www.php.net/chop


B<PHP::Strings::chop WILL NOT BE IMPLEMENTED>.

PHP's C<chop> function is an alias to its L<"rtrim"> function.

Perl has a builtin named L<chop|perlfunc/"chop">. Thus we do
not support the use of C<chop> as an alias to L<"rtrim">.


=cut


# No fn export due to clash with reserved perl keyword.


=head1 chr

http://www.php.net/chr


B<PHP::Strings::chr WILL NOT BE IMPLEMENTED>.

PHP's and Perl's L<chr|perlfunc/"chr"> functions operate sufficiently
identically.

Note that PHP's claims an ASCII value as input. Perl assumes Unicode.
But ensure you see L<the documentation|perlfunc/"chr"> for a precise
definition.

Note that it returns B<one character>, which in some string
encodings may not necessarily be B<one byte>.


=cut


# No fn export due to clash with reserved perl keyword.


=head1 chunk_split

http://www.php.net/chunk_split


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



=head1 convert_cyr_string

http://www.php.net/convert_cyr_string


=cut

sub convert_cyr_string {
    death(<<'EODEATH');

=pod

B<PHP::Strings::convert_cyr_string WILL NOT BE IMPLEMENTED>.

Perl has the L<Encode> module to convert between character encodings.

=cut

EODEATH
}



BEGIN { push @badeggs, "convert_cyr_string" };


=head1 count_chars

http://www.php.net/count_chars


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



=head1 crc32

http://www.php.net/crc32


TBD

=cut




BEGIN { $EXPORT_TAGS{crc32} = [ qw(
    crc32 
) ] }

sub crc32 { croak "TBD" }


=head1 crypt

http://www.php.net/crypt


B<PHP::Strings::crypt WILL NOT BE IMPLEMENTED>.

PHP's crypt is the same as Perl's. Thus there's no need for
C<PHP::String> to provide an implementation.

The C<CRYPT_*> constants are not provided.


=cut


# No fn export due to clash with reserved perl keyword.


=head1 echo

http://www.php.net/echo


=cut

sub echo {
    death(<<'EODEATH');

=pod

B<PHP::Strings::echo WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"print">.

=cut

EODEATH
}



BEGIN { push @badeggs, "echo" };


=head1 explode

http://www.php.net/explode


=cut

sub explode {
    death(<<'EODEATH');

=pod

B<PHP::Strings::explode WILL NOT BE IMPLEMENTED>.

Use the C<\Q> regex metachar and L<split|perlfunc/"split">.

    my @pieces = split /\Q$separator/, $string, $limit;

See L<perlfunc/"split"> for more details.

Note that C<split //> will split between every character, rather than
returning false. Note also that C<split "..."> is the same as
C<split /.../> which means to split everywhere three characters are
matched. The first argument to C<split> is always a regex.


=cut

EODEATH
}



BEGIN { push @badeggs, "explode" };


=head1 fprintf

http://www.php.net/fprintf


=cut

sub fprintf {
    death(<<'EODEATH');

=pod

B<PHP::Strings::fprintf WILL NOT BE IMPLEMENTED>.

Perl's L<printf|perlfunc/"printf"> can be told to which file handle to
print.

    printf FILEHANDLE $format, @args;

See L<perlfunc/"printf"> and L<perlfunc/"print"> for details.


=cut

EODEATH
}



BEGIN { push @badeggs, "fprintf" };


=head1 get_html_translation_table

http://www.php.net/get_html_translation_table


=cut

sub get_html_translation_table {
    death(<<'EODEATH');

=pod

B<PHP::Strings::get_html_translation_table WILL NOT BE IMPLEMENTED>.

Use the L<HTML::Entities> module to escape and unescape characters.

=cut

EODEATH
}



BEGIN { push @badeggs, "get_html_translation_table" };


=head1 hebrev

http://www.php.net/hebrev


=cut

sub hebrev {
    death(<<'EODEATH');

=pod

B<PHP::Strings::hebrev WILL NOT BE IMPLEMENTED>.

Use the L<Encode> module to convert between character encodings.

=cut

EODEATH
}



BEGIN { push @badeggs, "hebrev" };


=head1 hebrevc

http://www.php.net/hebrevc


=cut

sub hebrevc {
    death(<<'EODEATH');

=pod

B<PHP::Strings::hebrevc WILL NOT BE IMPLEMENTED>.

Use the L<Encode> module to convert between character encodings.

=cut

EODEATH
}



BEGIN { push @badeggs, "hebrevc" };


=head1 html_entity_decode

http://www.php.net/html_entity_decode


=cut

sub html_entity_decode {
    death(<<'EODEATH');

=pod

B<PHP::Strings::html_entity_decode WILL NOT BE IMPLEMENTED>.

Use the L<HTML::Entities> module to decode character entities.

=cut

EODEATH
}



BEGIN { push @badeggs, "html_entity_decode" };


=head1 htmlentities

http://www.php.net/htmlentities


=cut

sub htmlentities {
    death(<<'EODEATH');

=pod

B<PHP::Strings::htmlentities WILL NOT BE IMPLEMENTED>.

Use the L<HTML::Entities> module to encode character entities.

=cut

EODEATH
}



BEGIN { push @badeggs, "htmlentities" };


=head1 htmlspecialchars

http://www.php.net/htmlspecialchars


=cut

sub htmlspecialchars {
    death(<<'EODEATH');

=pod

B<PHP::Strings::htmlspecialchars WILL NOT BE IMPLEMENTED>.

Use the L<HTML::Entities> module to encode character entities.

=cut

EODEATH
}



BEGIN { push @badeggs, "htmlspecialchars" };


=head1 implode

http://www.php.net/implode


=cut

sub implode {
    death(<<'EODEATH');

=pod

B<PHP::Strings::implode WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"join">. Note that join cannot accept its arguments in
either order because that's just not how Perl arrays and lists work.
Note also that the joining sequence is not optional.


=cut

EODEATH
}



BEGIN { push @badeggs, "implode" };


=head1 join

http://www.php.net/join


B<PHP::Strings::join WILL NOT BE IMPLEMENTED>.

PHP's C<join> is an alias for C<implode>. See L<"implode">.


=cut


# No fn export due to clash with reserved perl keyword.


=head1 levenshtein

http://www.php.net/levenshtein


=cut

sub levenshtein {
    death(<<'EODEATH');

=pod

B<PHP::Strings::levenshtein WILL NOT BE IMPLEMENTED>.

I have no idea why PHP has this function.

See L<Text::Levenshtein>, L<Text::LevenshteinXS>, L<String::Approx>,
L<Text::PHraseDistance> and probably any number of other modules on
CPAN.


=cut

EODEATH
}



BEGIN { push @badeggs, "levenshtein" };


=head1 ltrim

http://www.php.net/ltrim


=cut

sub ltrim {
    death(<<'EODEATH');

=pod

B<PHP::Strings::ltrim WILL NOT BE IMPLEMENTED>.

As per L<perlfaq>:

    $string =~ s/^\s+//;

A basic glance through L<perlretut> or L<perlreref> should give you an
idea on how to change what characters get trimmed.


=cut

EODEATH
}



BEGIN { push @badeggs, "ltrim" };


=head1 md5

http://www.php.net/md5


=cut

sub md5 {
    death(<<'EODEATH');

=pod

B<PHP::Strings::md5 WILL NOT BE IMPLEMENTED>.

See L<Digest::MD5> which provides a number of functions for computing
MD5 hashes from various sources and to various formats.

Note: the user notes for this function at http://www.php.net/md5 are
among the most unintentionally funny and misinformed I've read.


=cut

EODEATH
}



BEGIN { push @badeggs, "md5" };


=head1 md5_file

http://www.php.net/md5_file


=cut

sub md5_file {
    death(<<'EODEATH');

=pod

B<PHP::Strings::md5_file WILL NOT BE IMPLEMENTED>.

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

EODEATH
}



BEGIN { push @badeggs, "md5_file" };


=head1 metaphone

http://www.php.net/metaphone


=cut

sub metaphone {
    death(<<'EODEATH');

=pod

B<PHP::Strings::metaphone WILL NOT BE IMPLEMENTED>.

L<Text::Metaphone> and L<Text::DoubleMetaphone> and
L<Text::TransMetaphone> all provide metaphonic calculations.


=cut

EODEATH
}



BEGIN { push @badeggs, "metaphone" };


=head1 money_format

http://www.php.net/money_format


sprintf for money.

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



=head1 nl2br

http://www.php.net/nl2br


=cut

sub nl2br {
    death(<<'EODEATH');

=pod

B<PHP::Strings::nl2br WILL NOT BE IMPLEMENTED>.

This is trivially implemented as:

    s,$,<br />,mg;


=cut

EODEATH
}



BEGIN { push @badeggs, "nl2br" };


=head1 nl_langinfo

http://www.php.net/nl_langinfo


=cut

sub nl_langinfo {
    death(<<'EODEATH');

=pod

B<PHP::Strings::nl_langinfo WILL NOT BE IMPLEMENTED>.

L<I18N::Langinfo> has a C<langinfo> command that corresponds to PHP's
C<nl_langinfo> function.


=cut

EODEATH
}



BEGIN { push @badeggs, "nl_langinfo" };


=head1 number_format

http://www.php.net/number_format


TBD

=cut




BEGIN { $EXPORT_TAGS{number_format} = [ qw(
    number_format 
) ] }

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



=head1 ord

http://www.php.net/ord


B<PHP::Strings::ord WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"ord">. Note that Perl returns Unicode value, not ASCII.

=cut


# No fn export due to clash with reserved perl keyword.


=head1 parse_str

http://www.php.net/parse_str


=cut

sub parse_str {
    death(<<'EODEATH');

=pod

B<PHP::Strings::parse_str WILL NOT BE IMPLEMENTED>.

See instead the L<CGI> and L<URI> modules which handles that sort of
thing.


=cut

EODEATH
}



BEGIN { push @badeggs, "parse_str" };


=head1 print

http://www.php.net/print


B<PHP::Strings::print WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"print">.


=cut


# No fn export due to clash with reserved perl keyword.


=head1 printf

http://www.php.net/printf


B<PHP::Strings::printf WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"printf">.


=cut


# No fn export due to clash with reserved perl keyword.


=head1 quoted_printable_decode

http://www.php.net/quoted_printable_decode


=cut

sub quoted_printable_decode {
    death(<<'EODEATH');

=pod

B<PHP::Strings::quoted_printable_decode WILL NOT BE IMPLEMENTED>.

L<MIME::QuotedPrint> provides functions for encoding and decoding
quoted-printable strings.


=cut

EODEATH
}



BEGIN { push @badeggs, "quoted_printable_decode" };


=head1 quotemeta

http://www.php.net/quotemeta


B<PHP::Strings::quotemeta WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"quotemeta">.

=cut


# No fn export due to clash with reserved perl keyword.


=head1 rtrim

http://www.php.net/rtrim


=cut

sub rtrim {
    death(<<'EODEATH');

=pod

B<PHP::Strings::rtrim WILL NOT BE IMPLEMENTED>.

Another trivial regular expression:

    $string =~ s/\s+$//;

See the notes on L<"ltrim">.


=cut

EODEATH
}



BEGIN { push @badeggs, "rtrim" };


=head1 setlocale

http://www.php.net/setlocale


=cut

sub setlocale {
    death(<<'EODEATH');

=pod

B<PHP::Strings::setlocale WILL NOT BE IMPLEMENTED>.

C<setlocale> is provided by the L<POSIX> module.

=cut

EODEATH
}



BEGIN { push @badeggs, "setlocale" };


=head1 sha1

http://www.php.net/sha1


=cut

sub sha1 {
    death(<<'EODEATH');

=pod

B<PHP::Strings::sha1 WILL NOT BE IMPLEMENTED>.

See L<"md5">, mentally substituting L<Digest::SHA1> for L<Digest::MD5>,
although the user notes are not as funny.


=cut

EODEATH
}



BEGIN { push @badeggs, "sha1" };


=head1 sha1_file

http://www.php.net/sha1_file


=cut

sub sha1_file {
    death(<<'EODEATH');

=pod

B<PHP::Strings::sha1_file WILL NOT BE IMPLEMENTED>.

See L<"md5_file">

=cut

EODEATH
}



BEGIN { push @badeggs, "sha1_file" };


=head1 similar_text

http://www.php.net/similar_text


TBD

=cut




BEGIN { $EXPORT_TAGS{similar_text} = [ qw(
    similar_text 
) ] }

sub similar_text { croak "TBD" }


=head1 soundex

http://www.php.net/soundex


=cut

sub soundex {
    death(<<'EODEATH');

=pod

B<PHP::Strings::soundex WILL NOT BE IMPLEMENTED>.

See L<Text::Soundex>, which also happens to be a core module.

=cut

EODEATH
}



BEGIN { push @badeggs, "soundex" };


=head1 sprintf

http://www.php.net/sprintf


B<PHP::Strings::sprintf WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"sprintf">.

=cut


# No fn export due to clash with reserved perl keyword.


=head1 sscanf

http://www.php.net/sscanf


=cut

sub sscanf {
    death(<<'EODEATH');

=pod

B<PHP::Strings::sscanf WILL NOT BE IMPLEMENTED>.

This is a godawful function. You should be using regular expressions
instead. See L<perlretut> and L<perlre>.


=cut

EODEATH
}



BEGIN { push @badeggs, "sscanf" };


=head1 str_ireplace

http://www.php.net/str_ireplace


=cut

sub str_ireplace {
    death(<<'EODEATH');

=pod

B<PHP::Strings::str_ireplace WILL NOT BE IMPLEMENTED>.

Use the C<s///> operator instead. See L<perlop> and L<perlre> for
details.


=cut

EODEATH
}



BEGIN { push @badeggs, "str_ireplace" };


=head1 str_pad

http://www.php.net/str_pad


TBD

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



=head1 str_repeat

http://www.php.net/str_repeat


=cut

sub str_repeat {
    death(<<'EODEATH');

=pod

B<PHP::Strings::str_repeat WILL NOT BE IMPLEMENTED>.

Instead, use the C<x> operator. See L<perlop> for details.

    my $by_ten = "-=" x 10;


=cut

EODEATH
}



BEGIN { push @badeggs, "str_repeat" };


=head1 str_replace

http://www.php.net/str_replace


=cut

sub str_replace {
    death(<<'EODEATH');

=pod

B<PHP::Strings::str_replace WILL NOT BE IMPLEMENTED>.

See the C<s///> operator. L<perlop> and L<perlre> have details.


=cut

EODEATH
}



BEGIN { push @badeggs, "str_replace" };


=head1 str_rot13

http://www.php.net/str_rot13


=cut

sub str_rot13 {
    death(<<'EODEATH');

=pod

B<PHP::Strings::str_rot13 WILL NOT BE IMPLEMENTED>.

This is rather trivially implemented as:

    $message =~ tr/A-Za-z/N-ZA-Mn-za-m/

(As per "Programming Perl", 3rd edition, section 5.2.4.)


=cut

EODEATH
}



BEGIN { push @badeggs, "str_rot13" };


=head1 str_shuffle

http://www.php.net/str_shuffle


Implemented, against my better judgement. It's trivial, like so many of
the others.


=cut




BEGIN { $EXPORT_TAGS{str_shuffle} = [ qw(
    str_shuffle 
) ] }

sub str_shuffle
{
    my ( $string ) = validate_pos( @_, STRING );

    return join '', shuffle split //, $string;
}



=head1 str_split

http://www.php.net/str_split


=cut

sub str_split {
    death(<<'EODEATH');

=pod

B<PHP::Strings::str_split WILL NOT BE IMPLEMENTED>.

See L<perlfunc/"split"> for details.

    my @bits = split /(.{,$len})/, $string;


=cut

EODEATH
}



BEGIN { push @badeggs, "str_split" };


=head1 str_word_count

http://www.php.net/str_word_count


TBD

=cut




BEGIN { $EXPORT_TAGS{str_word_count} = [ qw(
    str_word_count 
) ] }

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



=head1 strcasecmp

http://www.php.net/strcasecmp


=cut

sub strcasecmp {
    death(<<'EODEATH');

=pod

B<PHP::Strings::strcasecmp WILL NOT BE IMPLEMENTED>.

Equivalent to:

    lc($a) cmp lc($b)


=cut

EODEATH
}



BEGIN { push @badeggs, "strcasecmp" };


=head1 strchr

http://www.php.net/strchr


=cut

sub strchr {
    death(<<'EODEATH');

=pod

B<PHP::Strings::strchr WILL NOT BE IMPLEMENTED>.

See L<"strstr">

=cut

EODEATH
}



BEGIN { push @badeggs, "strchr" };


=head1 strcmp

http://www.php.net/strcmp


=cut

sub strcmp {
    death(<<'EODEATH');

=pod

B<PHP::Strings::strcmp WILL NOT BE IMPLEMENTED>.

Equivalent to:

    $a cmp $b


=cut

EODEATH
}



BEGIN { push @badeggs, "strcmp" };


=head1 strcoll

http://www.php.net/strcoll


=cut

sub strcoll {
    death(<<'EODEATH');

=pod

B<PHP::Strings::strcoll WILL NOT BE IMPLEMENTED>.

Equivalent to:

    use locale;

    $a cmp $b


=cut

EODEATH
}



BEGIN { push @badeggs, "strcoll" };


=head1 strcspn

http://www.php.net/strcspn


=cut

sub strcspn {
    death(<<'EODEATH');

=pod

B<PHP::Strings::strcspn WILL NOT BE IMPLEMENTED>.

Trivially equivalent to:

    my $cspn;
    $cspn = $-[0]-1 if $string =~ m/[chars]/;


=cut

EODEATH
}



BEGIN { push @badeggs, "strcspn" };


=head1 strip_tags

http://www.php.net/strip_tags


=cut

sub strip_tags {
    death(<<'EODEATH');

=pod

B<PHP::Strings::strip_tags WILL NOT BE IMPLEMENTED>.

You really want L<HTML::Scrubber>.

=cut

EODEATH
}



BEGIN { push @badeggs, "strip_tags" };


# ========================================================================

=head1 FUNCTIONS ACTUALLY IMPLEMENTED

Just in case you missed which functions were actually implemented
in that huge mass of unimplemented functions, here's the condensed list
of implemented functions:

=over 4


=item *

L<"addcslashes">

=item *

L<"chunk_split">

=item *

L<"count_chars">

=item *

L<"crc32">

=item *

L<"money_format">

=item *

L<"number_format">

=item *

L<"similar_text">

=item *

L<"str_pad">

=item *

L<"str_shuffle">

=item *

L<"str_word_count">

=back

=head1 BAD EGGS

All functions that I think are worthless are still exportable, with the
exception of any that would clash with a Perl builtin function.

If you try to actually use said function, a big fat error will result.

=cut

BEGIN {
    $EXPORT_TAGS{$_} = [ $_ ] for @badeggs;
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

If you want to implement some of the functions that I've
said will not be implemented, then I'll be happy to
include them.

=head1 BUGS, REQUESTS, COMMENTS

Log them via the CPAN RT system via the web or email:

    http://rt.cpan.org/NoAuth/ReportBug.html?Queue=PHP-Strings
    ( shorter URL: http://xrl.us/4at )

    bug-php-strings@rt.cpan.org

This makes it much easier for me to track things and thus means
your problem is less likely to be neglected.

=head1 THANKS

Juerd Waalboer (JUERD) for suggesting a link, and the assorted regex
functions.

Matthew Persico (PERSICOM) for the idea of having the
functions give their documentation as their error.

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
