use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'SWF::Builder',
    'VERSION_FROM' => 'lib/SWF/Builder.pm', # finds $VERSION
    'PREREQ_PM' => {
	'SWF::Element' => 0.29,
	'Font::TTF::Font' => 0.34,
    },
    'EXE_FILES' => ['scripts/asc.plx'],
);
