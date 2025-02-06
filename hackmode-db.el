;; (in-package :


(defvar hackmode-database nil "Current operation database object.")

(defvar hackmode-db-init-smt
  '("CREATE TABLE IF NOT EXISTS hosts (
    host_id INTEGER PRIMARY KEY,
    hostname TEXT,
    ip_address TEXT,
    UNIQUE(hostname, ip_address)
);"

    "CREATE TABLE IF NOT EXISTS domains (
    domain_id INTEGER PRIMARY KEY,
    domain_name TEXT NOT NULL UNIQUE
);"

    "CREATE TABLE IF NOT EXISTS host_domain (
    host_id INTEGER,
    domain_id INTEGER,
    PRIMARY KEY (host_id, domain_id),
    FOREIGN KEY (host_id) REFERENCES hosts(host_id),
    FOREIGN KEY (domain_id) REFERENCES domains(domain_id)
);"

    "CREATE TABLE IF NOT EXISTS ports (
    port_id INTEGER PRIMARY KEY,
    host_id INTEGER,
    port_number INTEGER NOT NULL,
    protocol TEXT NOT NULL,
    state TEXT NOT NULL,
    FOREIGN KEY (host_id) REFERENCES hosts(host_id)
);"

    "CREATE TABLE IF NOT EXISTS services (
    service_id INTEGER PRIMARY KEY,
    port_id INTEGER,
    service_name TEXT NOT NULL,
    version TEXT,
    FOREIGN KEY (port_id) REFERENCES ports(port_id)
);"

    "CREATE TABLE IF NOT EXISTS in_scope (
    scope_id INTEGER PRIMARY KEY,
    target TEXT NOT NULL,
    target_type TEXT NOT NULL
);"

    "CREATE TABLE IF NOT EXISTS out_of_scope (
    scope_id INTEGER PRIMARY KEY,
    target TEXT NOT NULL,
    target_type TEXT NOT NULL
);"

    "CREATE TABLE IF NOT EXISTS screenshots (
    screenshot_id INTEGER PRIMARY KEY,
    file_path TEXT NOT NULL,
    description TEXT,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    host_id INTEGER,
    port_id INTEGER,
    FOREIGN KEY (host_id) REFERENCES hosts(host_id),
    FOREIGN KEY (port_id) REFERENCES ports(port_id)
);"

    "CREATE TABLE urls (
    url_id INTEGER PRIMARY KEY,
    host_id INTEGER,
    port_id INTEGER,
    full_url TEXT NOT NULL,
    path TEXT,
    query_params TEXT,
    fragment TEXT,
    method TEXT,
    status_code INTEGER,
    content_type TEXT,
    response_size INTEGER,
    last_accessed DATETIME DEFAULT CURRENT_TIMESTAMP,
    notes TEXT,
    FOREIGN KEY (host_id) REFERENCES hosts(host_id),
    FOREIGN KEY (port_id) REFERENCES ports(port_id)
);")
  "List of sql statements for creating the sqlite database.")




(defun hackmode-db-init (database)
  "Create the schema for DATABASE object.."
  (mapcar (lambda (sql)
            (sqlite-execute database sql)) hackmode-db-init-smt))

(defun hackmode-db-open (path)
  "Open the asset tracking database at PATH."
  (sqlite-open path)
  (hackmode-db-init))




(provide 'hackmode-db)
;;; hackmode-db.el ends here
