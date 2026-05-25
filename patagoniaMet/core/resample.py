"""Time resampling utilities for aggregating daily/monthly data."""

import logging
from collections.abc import Callable

import pandas as pd

logger = logging.getLogger(__name__)


def monthly_resample(
    daily_data: pd.Series,
    days_min: int = 20,
    func: str | Callable = "mean",
) -> pd.Series:
    """Aggregate daily data to monthly resolution.

    Months with fewer than *days_min* valid observations are set to NaN.

    Args:
        daily_data: Daily time series with a DatetimeIndex.
        days_min: Minimum number of valid days required per month.
        func: Aggregation function name or callable passed to ``GroupBy.agg``.

    Returns:
        Monthly time series rounded to 3 decimal places.
    """
    if not isinstance(daily_data.index, pd.DatetimeIndex):
        daily_data = daily_data.copy()
        daily_data.index = pd.DatetimeIndex(daily_data.index)

    counts = daily_data.resample("MS").count()
    result = daily_data.resample("MS").agg(func)
    result[counts < days_min] = pd.NA
    return result.round(3)


def annual_resample(
    monthly_data: pd.Series,
    months_min: int = 10,
    func: str | Callable = "mean",
) -> pd.Series:
    """Aggregate monthly data to annual resolution.

    Years with fewer than *months_min* valid observations are set to NaN.

    Args:
        monthly_data: Monthly time series with a DatetimeIndex.
        months_min: Minimum number of valid months required per year.
        func: Aggregation function name or callable passed to ``GroupBy.agg``.

    Returns:
        Annual time series rounded to 3 decimal places.
    """
    if not isinstance(monthly_data.index, pd.DatetimeIndex):
        monthly_data = monthly_data.copy()
        monthly_data.index = pd.DatetimeIndex(monthly_data.index)

    counts = monthly_data.resample("YS").count()
    result = monthly_data.resample("YS").agg(func)
    result[counts < months_min] = pd.NA
    return result.round(3)
