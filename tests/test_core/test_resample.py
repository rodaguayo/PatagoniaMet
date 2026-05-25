"""Tests for patagoniaMet.core.resample."""

from datetime import datetime

import pandas as pd
import pytest

from patagoniaMet.core.resample import annual_resample, monthly_resample


@pytest.fixture
def daily_series() -> pd.Series:
    """Daily time series spanning 3 months without gaps."""
    idx = pd.date_range("2020-01-01", "2020-03-31", freq="D")
    return pd.Series(range(len(idx)), index=idx, dtype=float)


@pytest.fixture
def daily_series_gaps() -> pd.Series:
    """Daily series with a 10-day gap in February."""
    idx = pd.date_range("2020-01-01", "2020-03-31", freq="D")
    values = [float(i) if i < 31 or i >= 41 else pd.NA for i in range(len(idx))]
    return pd.Series(values, index=idx)


@pytest.fixture
def monthly_series() -> pd.Series:
    """Complete monthly series spanning 2 years."""
    idx = pd.date_range("2020-01-01", "2021-12-01", freq="MS")
    return pd.Series(range(len(idx)), index=idx, dtype=float)


class TestMonthlyResample:
    def test_basic_mean(self, daily_series: pd.Series) -> None:
        result = monthly_resample(daily_series)
        assert len(result) == 3  # Jan, Feb, Mar
        assert result.index[0] == datetime(2020, 1, 1)
        assert result.iloc[0] == pytest.approx(15.0)  # mean of 0..30
        assert result.iloc[1] == pytest.approx(45.0)  # mean of 31..59

    def test_custom_func_sum(self, daily_series: pd.Series) -> None:
        result = monthly_resample(daily_series, func="sum")
        assert result.iloc[0] == pytest.approx(465.0)  # sum of 0..30

    def test_days_min_threshold(self, daily_series_gaps: pd.Series) -> None:
        result = monthly_resample(daily_series_gaps, days_min=25)
        # January has 31 values, February has 19 valid (29 - 10 gap)
        assert pd.isna(result.iloc[1])  # Feb should be masked

    def test_low_threshold_passes(self, daily_series_gaps: pd.Series) -> None:
        result = monthly_resample(daily_series_gaps, days_min=15)
        assert pd.notna(result.iloc[1])  # Feb has 19 valid > 15

    def test_all_nan_input(self) -> None:
        idx = pd.date_range("2020-01-01", "2020-01-31", freq="D")
        series = pd.Series([pd.NA] * 31, index=idx)
        result = monthly_resample(series, days_min=1)
        assert pd.isna(result.iloc[0])

    def test_output_rounded(self, daily_series: pd.Series) -> None:
        result = monthly_resample(daily_series, func="sum")
        assert result.dtype == "float64"
        assert result.iloc[0] == 465.0


class TestAnnualResample:
    def test_basic_mean(self, monthly_series: pd.Series) -> None:
        result = annual_resample(monthly_series)
        assert len(result) == 2  # 2020, 2021
        assert result.index[0] == datetime(2020, 1, 1)
        assert result.iloc[0] == pytest.approx(5.5)  # mean of 0..11
        assert result.iloc[1] == pytest.approx(17.5)  # mean of 12..23

    def test_custom_func_sum(self, monthly_series: pd.Series) -> None:
        result = annual_resample(monthly_series, func="sum")
        assert result.iloc[0] == pytest.approx(66.0)  # sum of 0..11

    def test_months_min_threshold(self) -> None:
        idx = pd.date_range("2020-01-01", "2021-12-01", freq="MS")
        values = [float(i) if i < 6 else pd.NA for i in range(24)]
        series = pd.Series(values, index=idx)
        result = annual_resample(series, months_min=10)
        assert pd.isna(result.iloc[0])  # only 6 valid months
        assert pd.isna(result.iloc[1])  # 0 valid months

    def test_low_threshold_passes(self) -> None:
        idx = pd.date_range("2020-01-01", "2021-12-01", freq="MS")
        values = [float(i) if i < 10 else pd.NA for i in range(24)]
        series = pd.Series(values, index=idx)
        result = annual_resample(series, months_min=9)
        assert pd.notna(result.iloc[0])  # 10 valid months >= 9
        assert pd.isna(result.iloc[1])  # 0 valid months < 9

    def test_all_nan_input(self) -> None:
        idx = pd.date_range("2020-01-01", "2020-12-01", freq="MS")
        series = pd.Series([pd.NA] * 12, index=idx)
        result = annual_resample(series, months_min=1)
        assert pd.isna(result.iloc[0])

    def test_output_rounded(self, monthly_series: pd.Series) -> None:
        result = annual_resample(monthly_series, func="sum")
        assert result.iloc[0] == 66.0
