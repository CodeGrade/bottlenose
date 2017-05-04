#!/usr/bin/perl
use 5.10.0;
use Cwd;

my $string = shift or die "Usage:\n  $0 'string'\n";

die "Run me in rails root.\n" unless (-d 'public/submissions');
my $RAILS = getcwd();

say "Searching tarballs in submisison directory for string '$string'...";

my $files = `find public/submissions -name "*gz"`;

my $TMP = "/tmp/dredge.$$.tmp";
mkdir $TMP;

for my $path (split /\n/, $files) {
    $path =~ m{public/submissions/([^/]+)/};
    my $secret = $1;

    mkdir "$TMP/$secret";
    chdir "$TMP/$secret";

    system(qq{tar xzvf "$RAILS/$path"});

    my $results = `find . -exec grep -nH "$string" {} \\\;`;
    if ($results) {
        say "Found something in $path:";
        say $results;
    }
}
