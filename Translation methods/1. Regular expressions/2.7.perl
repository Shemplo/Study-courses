while (<>) {
	s/([A-Za-z])((\1)+)/$1/g;
	print ;
}