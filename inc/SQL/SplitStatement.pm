#line 1
package SQL::SplitStatement;

use Moose;

our $VERSION = '0.05003';
$VERSION = eval $VERSION;

use SQL::Tokenizer qw(tokenize_sql);
use List::MoreUtils qw(firstval each_array);

use constant {
    SEMICOLON     => ';',
    FORWARD_SLASH => '/',
    PLACEHOLDER   => '?'
};

my $transaction_re = qr[^(?:
    ;
    |/
    |WORK
    |TRAN
    |TRANSACTION
    |ISOLATION
    |READ
)$]xi;
my $procedural_END_re = qr/^(?:IF|LOOP)$/i;
my $terminator_re     = qr[;|/|;\s+/];
my $begin_comment_re  = qr/^(?:--|\/\*)/;
my $DECLARE_re        = qr/^(?:DECLARE|PROCEDURE|FUNCTION)$/i;
my $PACKAGE_re        = qr/^PACKAGE$/i;
my $BEGIN_re          = qr/^BEGIN$/i;
my $END_re            = qr/^END$/i;

my $CREATE_ALTER_re            = qr/^(?:CREATE|ALTER)$/i;
my $OR_REPLACE_re              = qr/^(?:OR|REPLACE)$/i;
my $OR_REPLACE_PACKAGE_BODY_re = qr/^(?:OR|REPLACE|PACKAGE|BODY)$/i;

my $BODY_re = qr/^BODY$/i;

has [ qw(
    keep_terminator
    keep_extra_spaces
    keep_empty_statements
    keep_comments
)] => (
    is      => 'rw',
    isa     => 'Bool',
    default => undef
);

# TODO: DEPRECATED, to remove!
has 'keep_semicolon' => (
    is      => 'rw',
    isa     => 'Bool',
    default => undef,
    trigger => \&_set_keep_terminator
);

sub _set_keep_terminator {
    my ($self, $value) = @_;
    $self->keep_terminator($value)
}

sub split {
    my ($self, $code) = @_;
    my ( $statements, undef ) = $self->split_with_placeholders($code);
    return @$statements
}

sub split_with_placeholders {
    my ($self, $code) = @_;
    
    my $statement = '';
    my @statements = ();
    my $inside_block = 0;
    my $inside_create_alter = 0;
    my $inside_declare = 0;
    my $inside_package = 0;
    my $package_name = '';
    my $statement_placeholders = 0;
    my @placeholders = ();
    
    my @tokens = tokenize_sql($code);
    
    while ( defined( my $token = shift @tokens ) ) {
        $statement .= $token
            unless $self->_is_comment($token) && ! $self->keep_comments;
        
        if ( $self->_is_BEGIN_of_block($token, \@tokens) ) {
            $inside_block++;
            $inside_declare = 0
        }
        elsif ( $token =~ $CREATE_ALTER_re ) {
            $inside_create_alter = 1;
            
            my $next_token
                = $self->_get_next_significant_token(\@tokens, $OR_REPLACE_re);
            
            if ( $next_token =~ $PACKAGE_re ) {
                $inside_package = 1;
                $package_name = $self->_get_package_name(\@tokens)
            }
        
        }
        elsif ( $token =~ $DECLARE_re ) {
            $inside_declare = 1
        }
        elsif ( my $name = $self->_is_END_of_block($token, \@tokens) ) {
            $inside_block-- if $inside_block;
            if ($name eq $package_name) {
                $inside_package = 0;
                $package_name = ''
            }
        }
        elsif ( $token eq PLACEHOLDER ) {
            $statement_placeholders++
        }
        elsif ( $self->_is_terminator($token, \@tokens) ) {
            $inside_create_alter = 0
        }
        
        next if ! $self->_is_terminator($token, \@tokens)
            || $inside_block || $inside_declare || $inside_package;
        
        push @statements, $statement;
        push @placeholders, $statement_placeholders;
        $statement = '';
        $statement_placeholders = 0;
    }
    push @statements, $statement;
    push @placeholders, $statement_placeholders;
    
    my ( @filtered_statements, @filtered_placeholders );
    
    if ( $self->keep_empty_statements ) {
        @filtered_statements   = @statements;
        @filtered_placeholders = @placeholders
    } else {
        my $sp = each_array( @statements, @placeholders );
        while ( my ($statement, $placeholder_num ) = $sp->() ) {
            unless ( $statement =~ /^\s*$terminator_re?\s*$/ ) {
                push @filtered_statements  , $statement;
                push @filtered_placeholders, $placeholder_num
            }
        }
    }
    
    unless ( $self->keep_terminator ) {
        s/$terminator_re$// foreach @filtered_statements
    }
    
    unless ( $self->keep_extra_spaces ) {
        s/^\s+|\s+$//g foreach @filtered_statements
    }
    
    return ( \@filtered_statements, \@filtered_placeholders )
}

sub _is_comment {
    my ($self, $token) = @_;
    return $token =~ $begin_comment_re
}

sub _is_BEGIN_of_block {
    my ($self, $token, $tokens) = @_;
    return 
        $token =~ $BEGIN_re
        && $self->_get_next_significant_token($tokens) !~ $transaction_re
}

sub _is_END_of_block {
    my ($self, $token, $tokens) = @_;
    my $next_token = $self->_get_next_significant_token($tokens);
    
    # Return possible package name
    return $next_token || 1
        if $token =~ $END_re && (
            ! defined($next_token)
            || $next_token !~ $procedural_END_re
        );
    
    return
}

sub _get_package_name {
    my ($self, $tokens) = @_;
    return $self->_get_next_significant_token(
        $tokens, $OR_REPLACE_PACKAGE_BODY_re
    )
}

sub _is_terminator {
    my ($self, $token, $tokens) = @_;
    
    return   if $token ne FORWARD_SLASH && $token ne SEMICOLON;
    return 1 if $token eq FORWARD_SLASH;
    
    # $token eq SEMICOLON
    my $next_token = $self->_get_next_significant_token($tokens);
    return 1 if ! defined($next_token) || $next_token ne FORWARD_SLASH;
    # $next_token eq FORWARD_SLASH
    return
}

sub _get_next_significant_token {
    my ($self, $tokens, $skiptoken_re) = @_;
    return $skiptoken_re
        ? firstval {
            /\S/ && ! $self->_is_comment($_) && ! /$skiptoken_re/
        } @$tokens
        : firstval {
            /\S/ && ! $self->_is_comment($_)
        } @$tokens
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;

__END__

#line 628
