package Wiki::Toolkit::Formatter::Mediawiki;

use warnings;
use strict;

=head1 NAME

Wiki::Toolkit::Formatter::Mediawiki - A Mediawiki-style formatter for
                                      Wiki::Toolkit.

=head1 VERSION

Version 0.02

=cut

use vars qw{$VERSION};
$VERSION = '0.02';

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
    my %defs = (store => undef);

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

# Turn [[Wiki Link|Title]], [URI Title], scheme:url, or StudlyCaps into links.
sub _make_html_link
{
    my ($tag, $opts, $tags) = @_;

    my $class = '';
    my ($href, $title);
    if ($tag =~ /^\[\[([^|#]*)(?:(#)([^|]*))?(?:(\|)(.*))?\]\]$/)
    {
	# Wiki link
	if ($1)
	{
	    $href = $opts->{prefix} . uri_escape $1 if $1;
	    $class = " class='link_wanted'"
		unless $tags->{_store} && $tags->{_store}->node_exists ($1);
	}

	$href .= $2 . uri_escape $3 if $2;

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
    elsif ($tag =~ /^\[(\S*)(?:(\s+)(.*))?\]$/)
    {
	# URI
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
    else
    {
	# Shouldn't be able to get here without either $opts->{absolute_links}
	# or $opts->{implicit_links};
	$tags->{_schema_regex} ||=
	    Text::MediawikiFormat::_make_schema_regex @{$tags->{schemas}};
	my $s = $tags->{_schema_regex};

	if ($tag =~ /^($s:\S+)$/)
	{
	    # absolute link
	    $href = $1;
	    $title = $1;
	}
	else
	{
	    # StudlyCaps
	    $href = $opts->{prefix} . uri_escape $tag;
	    $class = " class='link_wanted'"
		unless $tags->{_store} && $tags->{_store}->node_exists ($tag);
	    $title = $tag;
	}
    }

    return "<a$class href='$href'>$title</a>";
}

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

sub _format_redirect
{
    my ($self, $target) = @_;

#    my $href = _make_html_link ("[$target]", $tags, $opts);
    my $href = $target;

    my $img;
#    if ($tags->{_redirect_image})
#    {
#	$img = "<img alt='#REDIRECT' src='" . $tags->{_redirect_image}
#	. " />";
#    }
#    else
    {
	$img = "#REDIRECT ";
    }

    return "<span class='redirect_text'>$img$href</span>"
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

    my %tags;
    $tags{allowed_tags} = $self->{_allowed_tags}
	if $self->{_allowed_attrs};
    $tags{allowed_attrs} = $self->{_allowed_attrs}
	if $self->{_allowed_attrs};
    $tags{link} = \&_make_html_link;
    $tags{_store} = $self->{_store};

    my %opts;
    $opts{prefix} = $self->{_node_prefix}
	if $self->{_node_prefix};

    return wikiformat ($raw, \%tags, \%opts);
}

=head2 find_internal_links

  my @links_to = $formatter->find_internal_links ($content);

Returns a list of all nodes that the supplied content links to.

B<This is still broken.>

=cut

use vars qw{@_links_found};
@_links_found = ();
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
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Wiki-Toolkit-Formatter-Mediawiki>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Wiki::Toolkit::Formatter::Mediawiki

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Wiki-Toolkit-Formatter-Mediawiki>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Wiki-Toolkit-Formatter-Mediawiki>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Wiki-Toolkit-Formatter-Mediawiki>

=item * Search CPAN

L<http://search.cpan.org/dist/Wiki-Toolkit-Formatter-Mediawiki>

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
