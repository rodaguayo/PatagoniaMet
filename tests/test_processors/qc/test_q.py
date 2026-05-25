"""Tests for patagoniaMet.processors.qc.q."""

import pandas as pd
import pytest

from patagoniaMet.processors.qc.q import (
    BAD_STATIONS,
    MANUAL_DATE_MASKS,
    MANUAL_VALUE_MASKS,
    mask_constant,
    mask_isolated,
)


@pytest.fixture
def daily_df() -> pd.DataFrame:
    idx = pd.date_range("2020-01-01", "2020-12-31", freq="D")
    return pd.DataFrame({"A": range(len(idx))}, index=idx, dtype=float)


@pytest.fixture
def df_with_gaps() -> pd.DataFrame:
    idx = pd.date_range("2020-01-01", "2020-12-31", freq="D")
    values = [float(i) if i < 200 or i >= 320 else pd.NA for i in range(len(idx))]
    return pd.DataFrame({"A": values}, index=idx)


class TestMaskIsolated:
    def test_no_gaps_unchanged(self, daily_df: pd.DataFrame) -> None:
        result = mask_isolated(daily_df, window=30, max_na=0.5)
        pd.testing.assert_series_equal(result["A"], daily_df["A"])

    def test_large_gap_masked(self, df_with_gaps: pd.DataFrame) -> None:
        result = mask_isolated(df_with_gaps, window=30, max_na=0.5)
        assert result["A"].isna().sum() >= df_with_gaps["A"].isna().sum()

    def test_multiple_columns(self) -> None:
        idx = pd.date_range("2020-01-01", "2020-02-29", freq="D")
        df = pd.DataFrame({
            "X": [1.0] * 30 + [pd.NA] * 30,
            "Y": [2.0] * 60,
        }, index=idx)
        result = mask_isolated(df, window=30, max_na=0.4)
        assert result["X"].isna().sum() > 30  # second half should be masked
        assert result["Y"].isna().sum() == 0   # no gaps


class TestMaskConstant:
    def test_constant_series_masked(self) -> None:
        idx = pd.date_range("2020-01-01", "2020-02-29", freq="D")
        values = [5.0] * 30 + [float(i) for i in range(30)]
        df = pd.DataFrame({"A": values}, index=idx)
        result = mask_constant(df, window=15, max_cv=0.01)
        assert result["A"].iloc[7:22].isna().all()
        assert result["A"].iloc[-8:].notna().all()

    def test_varying_series_middle_unchanged(self, daily_df: pd.DataFrame) -> None:
        result = mask_constant(daily_df, window=30, max_cv=0.01)
        middle = result["A"].iloc[15:-15]
        assert middle.notna().all()

    def test_multiple_columns(self) -> None:
        idx = pd.date_range("2020-01-01", "2020-02-29", freq="D")
        df = pd.DataFrame({
            "X": [1.0] * 60,
            "Y": [float(i) for i in range(60)],
        }, index=idx)
        result = mask_constant(df, window=10, max_cv=0.01)
        assert result["X"].iloc[5:-5].isna().all()
        assert result["Y"].iloc[5:-5].notna().all()


class TestConstants:
    def test_bad_stations_not_empty(self) -> None:
        assert len(BAD_STATIONS) > 0
        assert all(isinstance(s, str) for s in BAD_STATIONS)

    def test_manual_date_masks_structure(self) -> None:
        for station, ranges in MANUAL_DATE_MASKS.items():
            assert isinstance(station, str)
            for start, end in ranges:
                assert start is None or isinstance(start, str)
                assert end is None or isinstance(end, str)

    def test_manual_value_masks_structure(self) -> None:
        for station, (op, val) in MANUAL_VALUE_MASKS.items():
            assert op in ("lt", "gt")
            assert isinstance(val, (int, float))
