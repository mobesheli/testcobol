.PHONY: install run test coverage lint format clean help

help:  ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Available targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  %-15s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

install:  ## Install dependencies
	poetry install

install-pip:  ## Install dependencies using pip
	pip install -r requirements.txt

run:  ## Run the application
	poetry run uvicorn accounting.api.main:app --reload

run-prod:  ## Run in production mode
	poetry run uvicorn accounting.api.main:app --host 0.0.0.0 --port 8000

test:  ## Run tests
	poetry run pytest

test-unit:  ## Run unit tests only
	poetry run pytest tests/unit/

test-parity:  ## Run parity tests only
	poetry run pytest tests/parity/

coverage:  ## Run tests with coverage report
	poetry run pytest --cov=src/accounting --cov-report=html --cov-report=term

lint:  ## Run linter
	poetry run ruff check src/ tests/

format:  ## Format code
	poetry run black src/ tests/
	poetry run ruff check --fix src/ tests/

typecheck:  ## Run type checker
	poetry run mypy src/

clean:  ## Clean generated files
	find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete
	find . -type f -name "*.pyo" -delete
	find . -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null || true
	rm -rf .pytest_cache htmlcov .coverage dist build

db-init:  ## Initialize database
	poetry run python -c "from accounting.infrastructure.database import create_tables; create_tables()"

all: install lint test  ## Install, lint, and test

.DEFAULT_GOAL := help

