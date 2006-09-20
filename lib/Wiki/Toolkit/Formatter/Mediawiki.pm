package Wiki::Toolkit::Formatter::Mediawiki;

use warnings;
use strict;

=head1 NAME

Wiki::Toolkit::Formatter::Mediawiki - A Mediawiki-style formatter for
                                      Wiki::Toolkit.

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

This package implements a formatter for the Wiki::Toolkit module which attempts
to duplicate the behavior of the Mediawiki application (a set of PHP scripts
used by Wikipedia and friends).

    use Wiki::Toolkit
    use Wiki::Toolkit::Store::Mediawiki;
    use Wiki::Toolkit::Formatter::Mediawiki;

    my $store = Wiki::Toolkit::Store::Mediawiki->new ( ... );
    # See below for parameter details.
    my $formatter = Wiki::Toolkit::Formatter::Mediawiki->new (%config,
                                                          store => $store);
    my $wiki = Wiki::Toolkit->new (store     => $store,
                               formatter => $formatter);

=cut



use Carp qw(croak);
use Text::MediawikiFormat as => 'wikiformat';
use URI::Escape;


=head1 METHODS

=head2 new

  my $store = Wiki::Toolkit::Store::Mediawiki->new ( ... );
  my $formatter = Wiki::Toolkit::Formatter::Mediawiki->new
	(allowed_tags => [# HTML
			  qw(b big blockquote br caption center cite code dd
			     div dl dt em font h1 h2 h3 h4 h5 h6 hr i li ol p
			     pre rb rp rt ruby s small strike strong sub sup
                             table td th tr tt u ul var),
			  # MediaWiki Specific
			  qw(nowiki),],
	 allowed_attrs => [qw(title align lang dir width height bgcolor),
			   qw(clear), # BR
			   qw(noshade), # HR
			   qw(cite), # BLOCKQUOTE, Q
			   qw(size face color), # FONT
			   # For various lists, mostly deprecated but
			   # safe
			   qw(type start value compact),
			   # Tables
			   qw(summary width border frame rules
			      cellspacing cellpadding valign char
			      charoff colgroup col span abbr axis
			      headers scope rowspan colspan),
			   qw(id class name style), # For CSS
			  ],
	 node_prefix => '',
	 store => $store);

Parameters will default to the values above, with the exception of C<store>,
which is a required argument without a default.  C<store> B<does not> have to
be of type C<Wiki::Toolkit::Store::Mediawiki>.

=cut

sub new
{
    my ($class, %args) = @_;
    croak "`store' is a required argument" unless $args{store};

    my $self = {};
    bless $self, $class;
    $self->_init (%args) or return undef;
    return $self;
}

sub _init
{
    my ($self, %args) = @_;

    # Store the parameters or their defaults.
    my %defs = (allowed_tags => [# HTML
				 qw(b big blockquote br caption center cite
				    code dd div dl dt em font h1 h2 h3 h4 h5 h6
				    hr i li ol p pre rb rp rt ruby s small
				    strike strong sub sup table td th tr tt u
				    ul var),
				 # MediaWiki Specific
				 qw(nowiki),],
		allowed_attrs => [qw(title align lang dir width height
				     bgcolor),
				  qw(clear), # BR
				  qw(noshade), # HR
				  qw(cite), # BLOCKQUOTE, Q
				  qw(size face color), # FONT
				  # For various lists, mostly deprecated but
				  # safe
				  qw(type start value compact),
				  # Tables
				  qw(summary width border frame rules
				     cellspacing cellpadding valign char
				     charoff colgroup col span abbr axis
				     headers scope rowspan colspan),
				  qw(id class name style), # For CSS
				 ],
		node_prefix => '',
		store => undef);

    foreach my $k (keys %defs)
    {
	$self->{"_".$k} = exists $args{$k} ? $args{$k} : $defs{$k};
    }

    return $self;
}



=head2 format

  my $html = $formatter->format ($content);

Escapes any tags which weren't specified as allowed on creation, then
interpolates any macros, then calls Text::WikiFormat::format (with the
specialized Mediawiki config) to translate the raw Wiki
language supplied into HTML.

=cut

# Turn the contents after a ; or : into a dictionary list.
# Using : without ; just looks like an indent.
sub _dl
{
    #my ($line, $indent, $lead) = @_;
    my ($term, $def);

    if ($_[2] eq ';')
    {
	if ($_[0] =~ /^(.*?)\s+:\s+(.*)$/)
	{
	    $term = $1;
	    $def = $2;
	}
    }
    else
    {
	*def = \$_[0];
    }

    my $retval;
    $retval = "<dt>$term</dt>\n" if defined $term;
    $retval .= "<dd>$def</dd>\n" if defined $def;
}

sub _make_wiki_link
{
}

# Turn [[Wiki Link|Title]] or [URI Title] into links.
sub _make_link
{
    my ($tag, $opts, $tags) = @_;

    my $class = '';
    my ($href, $title);
    if ($tag =~ /^\[([^|#]*)(?:(#)([^|]*))?(?:(\|)(.*))?\]$/)
    {
	# Wiki link
	print STDERR "_store = ", $tags->{_store}, "\n";
	if ($1)
	{
	    $href = $opts->{prefix} . uri_escape $1 if $1;
	    $class = " class='link_wanted'"
		unless $tags->{_store}->node_exists ($1);
	}

	if ($class)
	{
	    $href .= "?action=edit";
	}
	else
	{
	    $href .= $2 . uri_escape $3 if $2;
	}

	if ($4)
	{
	    # Title specified explicitly.
	    if (length $5)
	    {
		$title = $5;
	    }
	    else
	    {
		# An empty title asks Mediawiki to strip any parens off the end
		# of the node name.
		$1 =~ /^([^(]*)(?:\s*\()?/;
		$title = $1;
	    }
	}
	else
	{
	    # Title defaults to the node name.
	    $title = $1;
	}
    }
    else
    {
	# URI
	$tag =~ /^(\S*)(?:(\s+)(.*))?$/;
	$href = $1;
	if ($2)
	{
	    $title = $3;
	}
	else
	{
	    $title = ++$opts->{_uri_refs};
	}
	$href =~ s/'/%27/g;
    }

    return "<a$class href='$href'>"
	   . Text::MediawikiFormat::format_line ($title, $tags, $opts)
	   . "</a>";
}

# Store a TOC line for later.
#
# ASSUMPTIONS
#   $level >= 1
sub _store_toc_line
{
    my ($toc, $level, $title, $name) = @_;

    # TODO: Strip formatting from $title.

    if (@$toc && $level > $toc->[-1]->{level})
    {
	# Nest a sublevel.
	$toc->[-1]->{sublevel} = []
	    unless exists $toc->[-1]->{sublevel};
	_store_toc_line ($toc->[-1]->{sublevel}, $level, $title, $name);
    }
    else
    {
	push @$toc, {level => $level, title => $title, name => $name};
    }

    return $level;
}

# Make header text, storing the line for the TOC.
#
# ASSUMPTIONS
#   $tags->{_toc} has been initialized to an array ref.
sub _make_header
{
    my $level = length $_[2];
    my $n = uri_escape $_[3];

    _store_toc_line ($_[-2]->{_toc}, $level, $_[3], $n);

    return "<a name='$n'></a><h$level>",
	   Text::MediawikiFormat::format_line ($_[3], @_[-2, -1]),
	   "</h$level>\n";
}

#sub _format_redirect
#{
#    my ($self, $target) = @_;
#
#    my $href = _make_link ("[$target]", $tags, $opts);
#
#    if ($tags->{_redirect_image})
#    {
#	$img = "<img alt='#REDIRECT' src='" . $tags->{_redirect_image}
#	       . " />";
#    }
#    else
#    {
#	$img = "#REDIRECT ";
#    }
#
#    return "<span class='redirect_text'>$img$href</span>"
#}

# Return the text of a page, after recursively replacing any variables
# and templates in the included page.
#
# Needs some sort of loop detection or, at least, a maximum number of includes
# to avoid DoS.
sub _include
{
}

sub _magic_words
{
}

sub format
{
    my ($self, $raw) = @_;

    # Special exception handling for redirects - Text::MediawikiFormat doesn't
    # know how to handle something that may only appear on the first line yet,
    # though it's going to need to if I intend to handle Mediawiki's full
    # template syntax.
    return $self->_format_redirect ($1)
	if $raw =~ /^#redirect\s+\[\[([^]]+)\]\]/si;

    return wikiformat ($raw,
		       {
			indent         => qr/^(?:[:*#;]*)(?=[:*#;])/,
			extended_link_delimiters =>
					  qr/\[(\[[^][]*\]|[^][]*)\]/,
			blocks         =>
			{
			 code          => qr/^ /,
			 header        => qr/^(=+)\s*(.+)\s*\1$/,
			 line          => qr/^-{4,}$/,
			 ordered       => qr/^#\s*/,
			 unordered     => qr/^\*\s*/,
			 definition    => qr/^([;:])\s*/,
			},

			indented       => {map {$_ => 1} qw(ordered unordered
							    definition)},
			nests          => {map {$_ => 1} qw(ordered unordered
							    definition)},
			blockorder     => [qw(code header line ordered
					      unordered definition paragraph)],

			link           => \&_make_link,
			code           => ['<pre>', "</pre>\n", '', "\n"],
			header         => ['', "\n", \&_make_header],
			ordered        => ["<ol>\n", "</ol>\n", '<li>',
					   "</li>\n"],
			definition     => ["<dl>\n", "</dl>\n", \&_dl],
			paragraph      => ["<p>", "</p>\n", '', "\n"],

			allowed_tags   => $self->{_allowed_tags},
			allowed_attrs  => $self->{_allowed_attrs},

			_store         => $self->{_store},
			_toc           => [],
                       },
		       {
			extended       => 1,
			implicit_links => 0,
			prefix         => $self->{_node_prefix},
			process_html   => 1,
		       },
		      );
}

=head2 find_internal_links

  my @links_to = $formatter->find_internal_links ($content);

Returns a list of all nodes that the supplied content links to.

=cut

our @_links_found = ();
sub find_internal_links {
    my ($self, $raw) = @_;

    my $foo = wikiformat ($raw,
	{ link =>
	  sub
	  {
	      my ($link, $opts) = @_;
	      $opts ||= {};
	      my $title;
	      ($link, $title) = split(/\|/, $link, 2) if $opts->{extended};
	      push @Wiki::Toolkit::Formatter::Default::_links_found, $link;
	      return ""; # don't care about output
	  }
	},
	{ extended       => $self->{_extended_links},
	  prefix         => $self->{_node_prefix},
	  implicit_links => $self->{_implicit_links} } );

    my @links = @_links_found;
    @_links_found = ();
    return @links;
}



=head1 SEE ALSO

=over 4

=item L<Wiki::Toolkit::Kwiki>

=item L<Wiki::Toolkit>

=item L<Wiki::Toolkit::Formatter::Default>

=item L<Wiki::Toolkit::Store::Mediawiki>

=back

=head1 AUTHOR

Derek R. Price, C<< <derek at ximbiot.com> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-cgi-wiki-formatter-mediawiki at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=CGI-Wiki-Formatter-Mediawiki>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Wiki::Toolkit::Formatter::Mediawiki

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/CGI-Wiki-Formatter-Mediawiki>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/CGI-Wiki-Formatter-Mediawiki>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=CGI-Wiki-Formatter-Mediawiki>

=item * Search CPAN

L<http://search.cpan.org/dist/CGI-Wiki-Formatter-Mediawiki>

=back

=head1 ACKNOWLEDGEMENTS

My thanks go to Kake Pugh, for providing the well written L<Wiki::Toolkit> and
L<Wiki::Toolkit::Kwiki> modules, which got me started on this.

=head1 COPYRIGHT & LICENSE

Copyright 2006 Derek R. Price, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of Wiki::Toolkit::Formatter::Mediawiki
