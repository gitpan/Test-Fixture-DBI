#line 1
#   -*- cperl -*-

package DBD::mysql;
use strict;
use vars qw(@ISA $VERSION $err $errstr $drh);

use DBI ();
use DynaLoader();
use Carp ();
@ISA = qw(DynaLoader);

$VERSION = '4.016';

bootstrap DBD::mysql $VERSION;


$err = 0;	# holds error code   for DBI::err
$errstr = "";	# holds error string for DBI::errstr
$drh = undef;	# holds driver handle once initialised

sub driver{
    return $drh if $drh;
    my($class, $attr) = @_;

    $class .= "::dr";

    # not a 'my' since we use it above to prevent multiple drivers
    $drh = DBI::_new_drh($class, { 'Name' => 'mysql',
				   'Version' => $VERSION,
				   'Err'    => \$DBD::mysql::err,
				   'Errstr' => \$DBD::mysql::errstr,
				   'Attribution' => 'DBD::mysql by Patrick Galbraith'
				 });

    $drh;
}

sub CLONE {
  undef $drh;
}

sub _OdbcParse($$$) {
    my($class, $dsn, $hash, $args) = @_;
    my($var, $val);
    if (!defined($dsn)) {
	return;
    }
    while (length($dsn)) {
	if ($dsn =~ /([^:;]*)[:;](.*)/) {
	    $val = $1;
	    $dsn = $2;
	} else {
	    $val = $dsn;
	    $dsn = '';
	}
	if ($val =~ /([^=]*)=(.*)/) {
	    $var = $1;
	    $val = $2;
	    if ($var eq 'hostname'  ||  $var eq 'host') {
		$hash->{'host'} = $val;
	    } elsif ($var eq 'db'  ||  $var eq 'dbname') {
		$hash->{'database'} = $val;
	    } else {
		$hash->{$var} = $val;
	    }
	} else {
	    foreach $var (@$args) {
		if (!defined($hash->{$var})) {
		    $hash->{$var} = $val;
		    last;
		}
	    }
	}
    }
}

sub _OdbcParseHost ($$) {
    my($class, $dsn) = @_;
    my($hash) = {};
    $class->_OdbcParse($dsn, $hash, ['host', 'port']);
    ($hash->{'host'}, $hash->{'port'});
}

sub AUTOLOAD {
    my ($meth) = $DBD::mysql::AUTOLOAD;
    my ($smeth) = $meth;
    $smeth =~ s/(.*)\:\://;

    my $val = constant($smeth, @_ ? $_[0] : 0);
    if ($! == 0) { eval "sub $meth { $val }"; return $val; }

    Carp::croak "$meth: Not defined";
}

1;


package DBD::mysql::dr; # ====== DRIVER ======
use strict;
use DBI qw(:sql_types);
use DBI::Const::GetInfoType;

sub connect {
    my($drh, $dsn, $username, $password, $attrhash) = @_;
    my($port);
    my($cWarn);
    my $connect_ref= { 'Name' => $dsn };
    my $dbi_imp_data;

    # Avoid warnings for undefined values
    $username ||= '';
    $password ||= '';
    $attrhash ||= {};

    # create a 'blank' dbh
    my($this, $privateAttrHash) = (undef, $attrhash);
    $privateAttrHash = { %$privateAttrHash,
	'Name' => $dsn,
	'user' => $username,
	'password' => $password
    };

    DBD::mysql->_OdbcParse($dsn, $privateAttrHash,
				    ['database', 'host', 'port']);

    
    if ($DBI::VERSION >= 1.49)
    {
      $dbi_imp_data = delete $attrhash->{dbi_imp_data};
      $connect_ref->{'dbi_imp_data'} = $dbi_imp_data;
    }

    if (!defined($this = DBI::_new_dbh($drh,
            $connect_ref,
            $privateAttrHash)))
    {
      return undef;
    }

    # Call msqlConnect func in mSQL.xs file
    # and populate internal handle data.
    DBD::mysql::db::_login($this, $dsn, $username, $password)
	  or $this = undef;

    if ($this && ($ENV{MOD_PERL} || $ENV{GATEWAY_INTERFACE})) {
        $this->{mysql_auto_reconnect} = 1;
    }
    $this;
}

sub data_sources {
    my($self) = shift;
    my($attributes) = shift;
    my($host, $port, $user, $password) = ('', '', '', '');
    if ($attributes) {
      $host = $attributes->{host} || '';
      $port = $attributes->{port} || '';
      $user = $attributes->{user} || '';
      $password = $attributes->{password} || '';
    }
    my(@dsn) = $self->func($host, $port, $user, $password, '_ListDBs');
    my($i);
    for ($i = 0;  $i < @dsn;  $i++) {
	$dsn[$i] = "DBI:mysql:$dsn[$i]";
    }
    @dsn;
}

sub admin {
    my($drh) = shift;
    my($command) = shift;
    my($dbname) = ($command eq 'createdb'  ||  $command eq 'dropdb') ?
	shift : '';
    my($host, $port) = DBD::mysql->_OdbcParseHost(shift(@_) || '');
    my($user) = shift || '';
    my($password) = shift || '';

    $drh->func(undef, $command,
	       $dbname || '',
	       $host || '',
	       $port || '',
	       $user, $password, '_admin_internal');
}

package DBD::mysql::db; # ====== DATABASE ======
use strict;
use DBI qw(:sql_types);

%DBD::mysql::db::db2ANSI = ("INT"   =>  "INTEGER",
			   "CHAR"  =>  "CHAR",
			   "REAL"  =>  "REAL",
			   "IDENT" =>  "DECIMAL"
                          );

### ANSI datatype mapping to mSQL datatypes
%DBD::mysql::db::ANSI2db = ("CHAR"          => "CHAR",
			   "VARCHAR"       => "CHAR",
			   "LONGVARCHAR"   => "CHAR",
			   "NUMERIC"       => "INTEGER",
			   "DECIMAL"       => "INTEGER",
			   "BIT"           => "INTEGER",
			   "TINYINT"       => "INTEGER",
			   "SMALLINT"      => "INTEGER",
			   "INTEGER"       => "INTEGER",
			   "BIGINT"        => "INTEGER",
			   "REAL"          => "REAL",
			   "FLOAT"         => "REAL",
			   "DOUBLE"        => "REAL",
			   "BINARY"        => "CHAR",
			   "VARBINARY"     => "CHAR",
			   "LONGVARBINARY" => "CHAR",
			   "DATE"          => "CHAR",
			   "TIME"          => "CHAR",
			   "TIMESTAMP"     => "CHAR"
			  );

sub prepare {
    my($dbh, $statement, $attribs)= @_;

    # create a 'blank' dbh
    my $sth = DBI::_new_sth($dbh, {'Statement' => $statement});

    # Populate internal handle data.
    if (!DBD::mysql::st::_prepare($sth, $statement, $attribs)) {
	$sth = undef;
    }

    $sth;
}

sub db2ANSI {
    my $self = shift;
    my $type = shift;
    return $DBD::mysql::db::db2ANSI{"$type"};
}

sub ANSI2db {
    my $self = shift;
    my $type = shift;
    return $DBD::mysql::db::ANSI2db{"$type"};
}

sub admin {
    my($dbh) = shift;
    my($command) = shift;
    my($dbname) = ($command eq 'createdb'  ||  $command eq 'dropdb') ?
	shift : '';
    $dbh->{'Driver'}->func($dbh, $command, $dbname, '', '', '',
			   '_admin_internal');
}

sub _SelectDB ($$) {
    die "_SelectDB is removed from this module; use DBI->connect instead.";
}

sub table_info ($) {
  my ($dbh, $catalog, $schema, $table, $type, $attr) = @_;
  $dbh->{mysql_server_prepare}||= 0;
  my $mysql_server_prepare_save= $dbh->{mysql_server_prepare};
  $dbh->{mysql_server_prepare}= 0;
  my @names = qw(TABLE_CAT TABLE_SCHEM TABLE_NAME TABLE_TYPE REMARKS);
  my @rows;

  my $sponge = DBI->connect("DBI:Sponge:", '','')
    or return $dbh->DBI::set_err($DBI::err, "DBI::Sponge: $DBI::errstr");

# Return the list of catalogs
  if (defined $catalog && $catalog eq "%" &&
      (!defined($schema) || $schema eq "") &&
      (!defined($table) || $table eq ""))
  {
    @rows = (); # Empty, because MySQL doesn't support catalogs (yet)
  }
  # Return the list of schemas
  elsif (defined $schema && $schema eq "%" &&
      (!defined($catalog) || $catalog eq "") &&
      (!defined($table) || $table eq ""))
  {
    my $sth = $dbh->prepare("SHOW DATABASES")
      or ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save && 
          return undef);

    $sth->execute()
      or ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save && 
        return DBI::set_err($dbh, $sth->err(), $sth->errstr()));

    while (my $ref = $sth->fetchrow_arrayref())
    {
      push(@rows, [ undef, $ref->[0], undef, undef, undef ]);
    }
  }
  # Return the list of table types
  elsif (defined $type && $type eq "%" &&
      (!defined($catalog) || $catalog eq "") &&
      (!defined($schema) || $schema eq "") &&
      (!defined($table) || $table eq ""))
  {
    @rows = (
        [ undef, undef, undef, "TABLE", undef ],
        [ undef, undef, undef, "VIEW",  undef ],
        );
  }
  # Special case: a catalog other than undef, "", or "%"
  elsif (defined $catalog && $catalog ne "" && $catalog ne "%")
  {
    @rows = (); # Nothing, because MySQL doesn't support catalogs yet.
  }
  # Uh oh, we actually have a meaty table_info call. Work is required!
  else
  {
    my @schemas;
    # If no table was specified, we want them all
    $table ||= "%";

    # If something was given for the schema, we need to expand it to
    # a list of schemas, since it may be a wildcard.
    if (defined $schema && $schema ne "")
    {
      my $sth = $dbh->prepare("SHOW DATABASES LIKE " .
          $dbh->quote($schema))
        or ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save && 
        return undef);
      $sth->execute()
        or ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save && 
        return DBI::set_err($dbh, $sth->err(), $sth->errstr()));

      while (my $ref = $sth->fetchrow_arrayref())
      {
        push @schemas, $ref->[0];
      }
    }
    # Otherwise we want the current database
    else
    {
      push @schemas, $dbh->selectrow_array("SELECT DATABASE()");
    }

    # Figure out which table types are desired
    my ($want_tables, $want_views);
    if (defined $type && $type ne "")
    {
      $want_tables = ($type =~ m/table/i);
      $want_views  = ($type =~ m/view/i);
    }
    else
    {
      $want_tables = $want_views = 1;
    }

    for my $database (@schemas)
    {
      my $sth = $dbh->prepare("SHOW /*!50002 FULL*/ TABLES FROM " .
          $dbh->quote_identifier($database) .
          " LIKE " .  $dbh->quote($table))
          or ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save && 
          return undef);

      $sth->execute() or
          ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save &&
          return DBI::set_err($dbh, $sth->err(), $sth->errstr()));

      while (my $ref = $sth->fetchrow_arrayref())
      {
        my $type = (defined $ref->[1] &&
            $ref->[1] =~ /view/i) ? 'VIEW' : 'TABLE';
        next if $type eq 'TABLE' && not $want_tables;
        next if $type eq 'VIEW'  && not $want_views;
        push @rows, [ undef, $database, $ref->[0], $type, undef ];
      }
    }
  }

  my $sth = $sponge->prepare("table_info",
  {
    rows          => \@rows,
    NUM_OF_FIELDS => scalar @names,
    NAME          => \@names,
  }) 
    or ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save && 
      return $dbh->DBI::set_err($sponge->err(), $sponge->errstr()));

  $dbh->{mysql_server_prepare}= $mysql_server_prepare_save;
  return $sth;
}

sub _ListTables {
  my $dbh = shift;
  if (!$DBD::mysql::QUIET) {
    warn "_ListTables is deprecated, use \$dbh->tables()";
  }
  return map { $_ =~ s/.*\.//; $_ } $dbh->tables();
}


sub column_info {
  my ($dbh, $catalog, $schema, $table, $column) = @_;
  $dbh->{mysql_server_prepare}||= 0;
  my $mysql_server_prepare_save= $dbh->{mysql_server_prepare};
  $dbh->{mysql_server_prepare}= 0;

  # ODBC allows a NULL to mean all columns, so we'll accept undef
  $column = '%' unless defined $column;

  my $ER_NO_SUCH_TABLE= 1146;

  my $table_id = $dbh->quote_identifier($catalog, $schema, $table);

  my @names = qw(
      TABLE_CAT TABLE_SCHEM TABLE_NAME COLUMN_NAME
      DATA_TYPE TYPE_NAME COLUMN_SIZE BUFFER_LENGTH DECIMAL_DIGITS
      NUM_PREC_RADIX NULLABLE REMARKS COLUMN_DEF
      SQL_DATA_TYPE SQL_DATETIME_SUB CHAR_OCTET_LENGTH
      ORDINAL_POSITION IS_NULLABLE CHAR_SET_CAT
      CHAR_SET_SCHEM CHAR_SET_NAME COLLATION_CAT COLLATION_SCHEM COLLATION_NAME
      UDT_CAT UDT_SCHEM UDT_NAME DOMAIN_CAT DOMAIN_SCHEM DOMAIN_NAME
      SCOPE_CAT SCOPE_SCHEM SCOPE_NAME MAX_CARDINALITY
      DTD_IDENTIFIER IS_SELF_REF
      mysql_is_pri_key mysql_type_name mysql_values
      mysql_is_auto_increment
      );
  my %col_info;

  local $dbh->{FetchHashKeyName} = 'NAME_lc';
  # only ignore ER_NO_SUCH_TABLE in internal_execute if issued from here
  my $desc_sth = $dbh->prepare("DESCRIBE $table_id " . $dbh->quote($column));
  my $desc = $dbh->selectall_arrayref($desc_sth, { Columns=>{} });

  #return $desc_sth if $desc_sth->err();
  if (my $err = $desc_sth->err())
  {
    # return the error, unless it is due to the table not 
    # existing per DBI spec
    if ($err != $ER_NO_SUCH_TABLE)
    {
      $dbh->{mysql_server_prepare}= $mysql_server_prepare_save;
      return undef;
    }
    $dbh->set_err(undef,undef);
    $desc = [];
  }

  my $ordinal_pos = 0;
  for my $row (@$desc)
  {
    my $type = $row->{type};
    $type =~ m/^(\w+)(?:\((.*?)\))?\s*(.*)/;
    my $basetype  = lc($1);
    my $typemod   = $2;
    my $attr      = $3;

    my $info = $col_info{ $row->{field} }= {
	    TABLE_CAT               => $catalog,
	    TABLE_SCHEM             => $schema,
	    TABLE_NAME              => $table,
	    COLUMN_NAME             => $row->{field},
	    NULLABLE                => ($row->{null} eq 'YES') ? 1 : 0,
	    IS_NULLABLE             => ($row->{null} eq 'YES') ? "YES" : "NO",
	    TYPE_NAME               => uc($basetype),
	    COLUMN_DEF              => $row->{default},
	    ORDINAL_POSITION        => ++$ordinal_pos,
	    mysql_is_pri_key        => ($row->{key}  eq 'PRI'),
	    mysql_type_name         => $row->{type},
      mysql_is_auto_increment => ($row->{extra} =~ /auto_increment/i ? 1 : 0),
    };
    #
	  # This code won't deal with a pathalogical case where a value
	  # contains a single quote followed by a comma, and doesn't unescape
	  # any escaped values. But who would use those in an enum or set?
    #
	  my @type_params= ($typemod && index($typemod,"'")>=0) ?
      ("$typemod," =~ /'(.*?)',/g)  # assume all are quoted
			: split /,/, $typemod||'';      # no quotes, plain list
	  s/''/'/g for @type_params;                # undo doubling of quotes

	  my @type_attr= split / /, $attr||'';

  	$info->{DATA_TYPE}= SQL_VARCHAR();
    if ($basetype =~ /^(char|varchar|\w*text|\w*blob)/)
    {
      $info->{DATA_TYPE}= SQL_CHAR() if $basetype eq 'char';
      if ($type_params[0])
      {
        $info->{COLUMN_SIZE} = $type_params[0];
      }
      else
      {
        $info->{COLUMN_SIZE} = 65535;
        $info->{COLUMN_SIZE} = 255        if $basetype =~ /^tiny/;
        $info->{COLUMN_SIZE} = 16777215   if $basetype =~ /^medium/;
        $info->{COLUMN_SIZE} = 4294967295 if $basetype =~ /^long/;
      }
    }
	  elsif ($basetype =~ /^(binary|varbinary)/)
    {
      $info->{COLUMN_SIZE} = $type_params[0];
	    # SQL_BINARY & SQL_VARBINARY are tempting here but don't match the
	    # semantics for mysql (not hex). SQL_CHAR &  SQL_VARCHAR are correct here.
	    $info->{DATA_TYPE} = ($basetype eq 'binary') ? SQL_CHAR() : SQL_VARCHAR();
    }
    elsif ($basetype =~ /^(enum|set)/)
    {
	    if ($basetype eq 'set')
      {
		    $info->{COLUMN_SIZE} = length(join ",", @type_params);
	    }
	    else
      {
        my $max_len = 0;
        length($_) > $max_len and $max_len = length($_) for @type_params;
        $info->{COLUMN_SIZE} = $max_len;
	    }
	    $info->{"mysql_values"} = \@type_params;
    }
    elsif ($basetype =~ /int/)
    { 
      # big/medium/small/tiny etc + unsigned?
	    $info->{DATA_TYPE} = SQL_INTEGER();
	    $info->{NUM_PREC_RADIX} = 10;
	    $info->{COLUMN_SIZE} = $type_params[0];
    }
    elsif ($basetype =~ /^decimal/)
    {
      $info->{DATA_TYPE} = SQL_DECIMAL();
      $info->{NUM_PREC_RADIX} = 10;
      $info->{COLUMN_SIZE}    = $type_params[0];
      $info->{DECIMAL_DIGITS} = $type_params[1];
    }
    elsif ($basetype =~ /^(float|double)/)
    {
	    $info->{DATA_TYPE} = ($basetype eq 'float') ? SQL_FLOAT() : SQL_DOUBLE();
	    $info->{NUM_PREC_RADIX} = 2;
	    $info->{COLUMN_SIZE} = ($basetype eq 'float') ? 32 : 64;
    }
    elsif ($basetype =~ /date|time/)
    { 
      # date/datetime/time/timestamp
	    if ($basetype eq 'time' or $basetype eq 'date')
      {
		    #$info->{DATA_TYPE}   = ($basetype eq 'time') ? SQL_TYPE_TIME() : SQL_TYPE_DATE();
        $info->{DATA_TYPE}   = ($basetype eq 'time') ? SQL_TIME() : SQL_DATE(); 
        $info->{COLUMN_SIZE} = ($basetype eq 'time') ? 8 : 10;
      }
	    else
      {
        # datetime/timestamp
        #$info->{DATA_TYPE}     = SQL_TYPE_TIMESTAMP();
		    $info->{DATA_TYPE}        = SQL_TIMESTAMP();
		    $info->{SQL_DATA_TYPE}    = SQL_DATETIME();
        $info->{SQL_DATETIME_SUB} = $info->{DATA_TYPE} - ($info->{SQL_DATA_TYPE} * 10);
        $info->{COLUMN_SIZE}      = ($basetype eq 'datetime') ? 19 : $type_params[0] || 14;
	    }
	    $info->{DECIMAL_DIGITS}= 0; # no fractional seconds
    }
    elsif ($basetype eq 'year')
    {	
      # no close standard so treat as int
	    $info->{DATA_TYPE}      = SQL_INTEGER();
	    $info->{NUM_PREC_RADIX} = 10;
	    $info->{COLUMN_SIZE}    = 4;
	  }
	  else
    {
	    Carp::carp("column_info: unrecognized column type '$basetype' of $table_id.$row->{field} treated as varchar");
    }
    $info->{SQL_DATA_TYPE} ||= $info->{DATA_TYPE};
    #warn Dumper($info);
  }

  my $sponge = DBI->connect("DBI:Sponge:", '','')
    or (  $dbh->{mysql_server_prepare}= $mysql_server_prepare_save &&
          return $dbh->DBI::set_err($DBI::err, "DBI::Sponge: $DBI::errstr"));

  my $sth = $sponge->prepare("column_info $table", {
      rows          => [ map { [ @{$_}{@names} ] } values %col_info ],
      NUM_OF_FIELDS => scalar @names,
      NAME          => \@names,
      }) or
  return ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save &&
          $dbh->DBI::set_err($sponge->err(), $sponge->errstr()));

  $dbh->{mysql_server_prepare}= $mysql_server_prepare_save;
  return $sth;
}


sub primary_key_info {
  my ($dbh, $catalog, $schema, $table) = @_;
  $dbh->{mysql_server_prepare}||= 0;
  my $mysql_server_prepare_save= $dbh->{mysql_server_prepare};

  my $table_id = $dbh->quote_identifier($catalog, $schema, $table);

  my @names = qw(
      TABLE_CAT TABLE_SCHEM TABLE_NAME COLUMN_NAME KEY_SEQ PK_NAME
      );
  my %col_info;

  local $dbh->{FetchHashKeyName} = 'NAME_lc';
  my $desc_sth = $dbh->prepare("SHOW KEYS FROM $table_id");
  my $desc= $dbh->selectall_arrayref($desc_sth, { Columns=>{} });
  my $ordinal_pos = 0;
  for my $row (grep { $_->{key_name} eq 'PRIMARY'} @$desc)
  {
    $col_info{ $row->{column_name} }= {
      TABLE_CAT   => $catalog,
      TABLE_SCHEM => $schema,
      TABLE_NAME  => $table,
      COLUMN_NAME => $row->{column_name},
      KEY_SEQ     => $row->{seq_in_index},
      PK_NAME     => $row->{key_name},
    };
  }

  my $sponge = DBI->connect("DBI:Sponge:", '','')
    or 
     ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save &&
      return $dbh->DBI::set_err($DBI::err, "DBI::Sponge: $DBI::errstr"));

  my $sth= $sponge->prepare("primary_key_info $table", {
      rows          => [ map { [ @{$_}{@names} ] } values %col_info ],
      NUM_OF_FIELDS => scalar @names,
      NAME          => \@names,
      }) or 
       ($dbh->{mysql_server_prepare}= $mysql_server_prepare_save &&
        return $dbh->DBI::set_err($sponge->err(), $sponge->errstr()));

  $dbh->{mysql_server_prepare}= $mysql_server_prepare_save;

  return $sth;
}


sub foreign_key_info {
    my ($dbh,
        $pk_catalog, $pk_schema, $pk_table,
        $fk_catalog, $fk_schema, $fk_table,
       ) = @_;

    # INFORMATION_SCHEMA.KEY_COLUMN_USAGE was added in 5.0.6
    my ($maj, $min, $point) = _version($dbh);
    return if $maj < 5 || ($maj == 5 && $point < 6);

    my $sql = <<'EOF';
SELECT NULL AS PKTABLE_CAT,
       A.REFERENCED_TABLE_SCHEMA AS PKTABLE_SCHEM,
       A.REFERENCED_TABLE_NAME AS PKTABLE_NAME,
       A.REFERENCED_COLUMN_NAME AS PKCOLUMN_NAME,
       A.TABLE_CATALOG AS FKTABLE_CAT,
       A.TABLE_SCHEMA AS FKTABLE_SCHEM,
       A.TABLE_NAME AS FKTABLE_NAME,
       A.COLUMN_NAME AS FKCOLUMN_NAME,
       A.ORDINAL_POSITION AS KEY_SEQ,
       NULL AS UPDATE_RULE,
       NULL AS DELETE_RULE,
       A.CONSTRAINT_NAME AS FK_NAME,
       NULL AS PK_NAME,
       NULL AS DEFERABILITY,
       NULL AS UNIQUE_OR_PRIMARY
  FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE A,
       INFORMATION_SCHEMA.TABLE_CONSTRAINTS B
 WHERE A.TABLE_SCHEMA = B.TABLE_SCHEMA AND A.TABLE_NAME = B.TABLE_NAME
   AND A.CONSTRAINT_NAME = B.CONSTRAINT_NAME AND B.CONSTRAINT_TYPE IS NOT NULL
EOF

    my @where;
    my @bind;

    # catalogs are not yet supported by MySQL

#    if (defined $pk_catalog) {
#        push @where, 'A.REFERENCED_TABLE_CATALOG = ?';
#        push @bind, $pk_catalog;
#    }

    if (defined $pk_schema) {
        push @where, 'A.REFERENCED_TABLE_SCHEMA = ?';
        push @bind, $pk_schema;
    }

    if (defined $pk_table) {
        push @where, 'A.REFERENCED_TABLE_NAME = ?';
        push @bind, $pk_table;
    }

#    if (defined $fk_catalog) {
#        push @where, 'A.TABLE_CATALOG = ?';
#        push @bind,  $fk_schema;
#    }

    if (defined $fk_schema) {
        push @where, 'A.TABLE_SCHEMA = ?';
        push @bind,  $fk_schema;
    }

    if (defined $fk_table) {
        push @where, 'A.TABLE_NAME = ?';
        push @bind,  $fk_table;
    }

    if (@where) {
        $sql .= ' AND ';
        $sql .= join ' AND ', @where;
    }
    $sql .= " ORDER BY A.TABLE_SCHEMA, A.TABLE_NAME, A.ORDINAL_POSITION";

    local $dbh->{FetchHashKeyName} = 'NAME_uc';
    my $sth = $dbh->prepare($sql);
    $sth->execute(@bind);

    return $sth;
}


sub _version {
    my $dbh = shift;

    return
        $dbh->get_info($DBI::Const::GetInfoType::GetInfoType{SQL_DBMS_VER})
            =~ /(\d+)\.(\d+)\.(\d+)/;
}


####################
# get_info()
# Generated by DBI::DBD::Metadata

sub get_info {
    my($dbh, $info_type) = @_;
    require DBD::mysql::GetInfo;
    my $v = $DBD::mysql::GetInfo::info{int($info_type)};
    $v = $v->($dbh) if ref $v eq 'CODE';
    return $v;
}



package DBD::mysql::st; # ====== STATEMENT ======
use strict;

1;

__END__

#line 1988


