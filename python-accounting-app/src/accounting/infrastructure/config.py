"""Application configuration using pydantic-settings."""

from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """
    Application settings.
    
    Maps to initial conditions from BRD:
    - Initial balance: 1,000.00 (docs/Brd.md line 20)
    - COBOL: STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00 (data.cob:6)
    """
    
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=False
    )
    
    # Database
    database_url: str = "sqlite:///./accounting.db"
    
    # Application
    app_env: str = "development"
    log_level: str = "INFO"
    
    # Business Configuration
    initial_balance_cents: int = 100_000  # 1,000.00 in cents (COBOL default)
    
    @property
    def is_production(self) -> bool:
        """Check if running in production."""
        return self.app_env == "production"
    
    @property
    def is_sqlite(self) -> bool:
        """Check if using SQLite database."""
        return self.database_url.startswith("sqlite")


# Global settings instance
settings = Settings()

