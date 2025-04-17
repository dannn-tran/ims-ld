CREATE TYPE monetary_amount AS (
  currency VARCHAR(3),
  value NUMERIC(9, 2)
);

-- Base objects

CREATE TABLE storages (
  id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  slug VARCHAR(64) UNIQUE,
  label TEXT,
  description TEXT
);

CREATE TABLE items (
  id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  slug VARCHAR(64) UNIQUE,
  label TEXT,
  acquire_date DATE,
  acquire_price monetary_amount,
  acquire_source TEXT,
  storage_id INT REFERENCES storages(id),
  details TEXT
);

CREATE UNIQUE INDEX item_by_slug ON items(slug);

CREATE TABLE entities (
  id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  slug VARCHAR(64) UNIQUE,
  label TEXT
);

CREATE UNIQUE INDEX entity_by_slug ON entities(slug);

CREATE TABLE collections (
  id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  slug VARCHAR(64) UNIQUE,
  label TEXT
);

CREATE UNIQUE INDEX collection_by_slug ON collections(slug);

-- Linkages between base objects

CREATE TABLE item_entity_reltypes (
  slug VARCHAR(64) PRIMARY KEY,
  label TEXT
);

CREATE TABLE item_entity_relationships (
  item_id INT REFERENCES items(id),
  entity_id INT REFERENCES entities(id),
  reltype VARCHAR(64) REFERENCES entity_item_reltypes(slug),
  PRIMARY KEY (item_id, entity_id, reltype)
);

CREATE TABLE item_in_collection_membership (
  collection_id INT REFERENCES collections(id),
  item_id INT REFERENCES items(id),
  PRIMARY KEY (collection_id, item_id)
);

CREATE TABLE collection_in_collection_membership (
  outer_id INT REFERENCES collections(id),
  inner_id INT REFERENCES items(id),
  PRIMARY KEY (outer_id, inner_id)
);

-- Tags

CREATE TABLE tags (
  slug VARCHAR(64) PRIMARY KEY,
  label TEXT
);

CREATE TABLE item_tag_prod (
  item_id INT REFERENCES items(id),
  tag VARCHAR(64) REFERENCES tags(slug),
  PRIMARY KEY (item_id, tag)
);

CREATE TABLE entity_tag_prod (
  entity_id INT REFERENCES entities(id),
  tag VARCHAR(64) REFERENCES tags(slug)
  PRIMARY KEY (entity_id, tag)
);

CREATE TABLE collection_tag_prod (
  collection_id INT REFERENCES collections(id),
  tag VARCHAR(64) REFERENCES tags(slug),
  PRIMARY KEY (collection_id, tag)
);