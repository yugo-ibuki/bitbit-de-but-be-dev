// Atlas HCL Schema Definition
// This is an alternative to schema.sql

schema "main" {
  comment = "Main schema for Spanner database"
}

// Users table
table "users" {
  schema = schema.main

  column "id" {
    type = bigint
    null = false
  }

  column "email" {
    type = varchar(255)
  }

  column "display_name" {
    type = varchar(255)
  }

  column "bio" {
    type = varchar(1024)
  }

  column "created_at" {
    type = timestamp
    null = false
  }

  column "updated_at" {
    type = timestamp
    null = false
  }

  primary_key {
    columns = [column.id]
  }
}

// Posts table
table "posts" {
  schema = schema.main

  column "id" {
    type = bigint
    null = false
  }

  column "title" {
    type = varchar(255)
    null = false
  }

  column "body" {
    type = text
  }

  column "author_id" {
    type = bigint
    null = false
  }

  column "published" {
    type = bool
    null = false
    default = false
  }

  column "created_at" {
    type = timestamp
    null = false
  }

  column "updated_at" {
    type = timestamp
    null = false
  }

  primary_key {
    columns = [column.id]
  }

  foreign_key "author_fk" {
    columns = [column.author_id]
    ref_columns = [table.users.column.id]
    on_delete = NO_ACTION
  }

  index "posts_by_author" {
    columns = [column.author_id]
  }

  index "posts_by_created_at" {
    columns = [column.created_at]
  }
}
