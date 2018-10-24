while (<>) {
	s/([a-z])\1/$1/gi;
	print ;
}