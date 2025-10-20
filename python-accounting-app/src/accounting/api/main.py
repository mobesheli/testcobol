"""Main application entry point."""

from accounting.api.app import app
from accounting.api.endpoints import router
from accounting.api.frontend import router as frontend_router

# Register routers
app.include_router(router)
app.include_router(frontend_router)

# Export for uvicorn
__all__ = ["app"]
