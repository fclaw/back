pg_dump:
	mkdir -p dumps && \
		PGPASSWORD="$$DB_PASS" \
		pg_dump -U "$$DB_USER" \
			-d "$$DB_NAME" \
			-p "$$DB_PORT" \
			-h "$$DB_HOST" \
			--format=custom \
			--compress=6 \
			-f dumps/my-dump-$$(date +%F+%H-%M-%S%z).pgdump

# Convenient for making diffs of database dumps to see what was changed
pg_dump_sql:
	mkdir -p dumps && \
		PGPASSWORD="$$DB_PASS" \
		pg_dump -U "$$DB_USER" \
			-d "$$DB_NAME" \
			-p "$$DB_PORT" \
			-h "$$DB_HOST" \
			-f dumps/my-sql-$$(date +%F+%H-%M-%S%z).sql

# Use make pg_restore FROM=path/to/dump
pg_restore:
	export PGHOST="$$DB_HOST" PGPORT="$$DB_PORT" PGUSER="$$DB_USER" PGPASSWORD="$$DB_PASS" && \
		pg_restore -d "$$DB_NAME" --no-owner --no-acl "$(FROM)