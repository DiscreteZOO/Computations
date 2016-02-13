UPDATE "xyz.discretezoo.core.graphs.Graph"
SET "vtIndex"="xyz.discretezoo.core.CatalogId"."index"
FROM "io.duality.PersistableSet@data"
JOIN "xyz.discretezoo.core.CatalogId"
ON "xyz.discretezoo.core.CatalogId"."@id" = 
"io.duality.PersistableSet@data"."@element"
WHERE "io.duality.PersistableSet@data"."@collection" = "xyz.discretezoo.core.graphs.Graph".catalogues
AND "xyz.discretezoo.core.CatalogId"."catalog" = 'vt'
AND "xyz.discretezoo.core.CatalogId"."index" > 0;