use 5.006;
use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'		=> 'PHP::Strings',
    'VERSION_FROM'	=> 'lib/PHP/Strings.pm',
    'PREREQ_PM'		=> {
        'Test::More'       => '0.45',
        'Params::Validate' => '0.70',
        'List::Util'       => '1.13',
        'Scalar::Util'     => '1.13',

    },
    ($] >= 5.005 ?
      (ABSTRACT_FROM => 'lib/PHP/Strings.pm',
       AUTHOR     => 'Iain Truskett <spoon@cpan.org>') : ()),
    'LIBS'		=> [''],
    'DEFINE'		=> '',
    'INC'		=> '-I.',
);

if  (eval {require ExtUtils::Constant; 1}) {
  # If you edit these definitions to change the constants used by this module,
  # you will need to use the generated const-c.inc and const-xs.inc
  # files to replace their "fallback" counterparts before distributing your
  # changes.
  my @names = (qw());
  ExtUtils::Constant::WriteConstants(
                                     NAME         => 'PHP::Strings',
                                     NAMES        => \@names,
                                     DEFAULT_TYPE => 'IV',
                                     C_FILE       => 'const-c.inc',
                                     XS_FILE      => 'const-xs.inc',
                                  );

}
else {
  use File::Copy;
  use File::Spec;
  foreach my $file ('const-c.inc', 'const-xs.inc') {
    my $fallback = File::Spec->catfile('fallback', $file);
    copy ($fallback, $file) or die "Can't copy $fallback to $file: $!";
  }
}
