while (<>) {
	s/(\w+)([^\w]+)(\w+)/$3$2$1/;
	print ;
}