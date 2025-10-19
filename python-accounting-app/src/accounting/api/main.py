"""Main application entry point."""

from accounting.api.app import app
from accounting.api.endpoints import router

# Register routers
app.include_router(router)

# Export for uvicorn
__all__ = ["app"]

