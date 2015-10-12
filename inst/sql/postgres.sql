select
  t.table_name as "table",
  c.column_name as "column",
  case when pk.column_name is null then 0 else 1 end as "key",
  fk.ref,
  fk.ref_col,
  case c.is_nullable when 'YES' then 0 else 1 end as "mandatory",
  c.data_type as "type",
  c.ordinal_position as column_order

from
  information_schema.columns c
  inner join information_schema.tables t on
    t.table_name = c.table_name
    and t.table_catalog = c.table_catalog
    and t.table_schema = c.table_schema

  left join  -- primary keys
  ( SELECT 
      tc.constraint_name, tc.table_name, kcu.column_name 
      FROM 
      information_schema.table_constraints AS tc 
      JOIN information_schema.key_column_usage AS kcu ON 
      tc.constraint_name = kcu.constraint_name
    WHERE constraint_type = 'PRIMARY KEY'
  ) pk on
    pk.table_name = c.table_name
    and pk.column_name = c.column_name

  left join  -- foreign keys
    ( SELECT 
        tc.constraint_name, kcu.table_name, kcu.column_name, 
        ccu.table_name as "ref",
        ccu.column_name as "ref_col" 
      FROM 
        information_schema.table_constraints AS tc 
        JOIN information_schema.key_column_usage AS kcu ON 
        tc.constraint_name = kcu.constraint_name
        JOIN information_schema.constraint_column_usage AS ccu ON 
        ccu.constraint_name = tc.constraint_name         
      WHERE tc.constraint_type = 'FOREIGN KEY'
    ) fk on
      fk.table_name = c.table_name
      and fk.column_name = c.column_name

where
  c.table_schema = 'public'
  and t.table_type = 'BASE TABLE'
