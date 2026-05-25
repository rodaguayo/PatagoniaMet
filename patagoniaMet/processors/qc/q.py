"""Quality control for daily streamflow (Q) data."""

import logging
from pathlib import Path

import numpy as np
import pandas as pd

from patagoniaMet.config import get_config
from patagoniaMet.core.resample import annual_resample, monthly_resample

logger = logging.getLogger(__name__)

BAD_STATIONS: list[str] = [
    "X10322003", "X00001815", "X00002315", "X00002230",
    "X00002218", "X00002239", "X00002825", "X00002822",
    "X12660001", "X12861001", "X12291001", "X00002828",
    "X11536001", "X11521001",
    "X10520001", "X00001870", "X10344004", "X10344003",
    "X00002202", "X11310002", "X12284006", "X12286002",
]

MANUAL_DATE_MASKS: dict[str, list[tuple[str | None, str | None]]] = {
    "X00001812": [("1997-01-01", "2000-01-01")],
    "X10411002": [("2000-01-01", "2003-01-01")],
    "X11143002": [("2016-01-01", "2017-01-01")],
    "X00002288": [("2006-01-01", "2008-01-01")],
    "X00002821": [("2000-01-01", "2001-01-01")],
    "X12452001": [("2015-01-01", "2016-01-01")],
    "X11310002": [("1990-01-01", "1995-01-01")],
    "X00002818": [("2015-01-01", None)],
    "X12289002": [("2020-01-01", None)],
    "X12876001": [("2020-01-01", None)],
    "X12585001": [("2020-01-01", None)],
}

MANUAL_VALUE_MASKS: dict[str, tuple[str, float]] = {
    "X11711000": ("lt", 100),
    "X11545000": ("lt", 250),
    "X11530000": ("lt", 200),
    "X11307001": ("gt", 170),
    "X12284007": ("gt", 200),
}


def mask_isolated(
    df: pd.DataFrame, window: int = 365, max_na: float = 0.8
) -> pd.DataFrame:
    """Mask values where the rolling window has too many NaNs.

    Args:
        df: Daily streamflow DataFrame with DatetimeIndex.
        window: Rolling window size in days.
        max_na: Maximum fraction of NaNs allowed in the window.

    Returns:
        DataFrame with isolated values masked to NaN.
    """
    result = df.copy()
    for col in result.columns:
        na_frac = result[col].isna().rolling(window, center=True).mean()
        result[col] = result[col].mask(na_frac > max_na)
    return result


def mask_constant(
    df: pd.DataFrame, window: int = 30, max_cv: float = 0.01
) -> pd.DataFrame:
    """Mask values where the rolling coefficient of variation is near zero.

    Args:
        df: Daily streamflow DataFrame with DatetimeIndex.
        window: Rolling window size in days.
        max_cv: Maximum CV threshold; windows with CV <= max_cv are masked.

    Returns:
        DataFrame with constant-period values masked to NaN.
    """
    result = df.copy()
    for col in result.columns:
        mean_ = result[col].rolling(window, center=True).mean()
        std_ = result[col].rolling(window, center=True).std(ddof=0)
        cv = std_ / mean_.where(mean_ != 0, np.nan)
        result[col] = result[col].mask(cv <= max_cv)
    return result


def run_q_pipeline(
    data_path: str | Path,
    meta_path: str | Path,
    output_dir: str | Path,
) -> None:
    """Run the full streamflow quality-control pipeline.

    Args:
        data_path: Path to the raw daily Q CSV.
        meta_path: Path to the station metadata CSV.
        output_dir: Directory to write cleaned CSVs.
    """
    config = get_config()
    qc_cfg = config.get("quality_check", {}).get("q", {})

    df = pd.read_csv(data_path, index_col=0, parse_dates=True)
    meta = pd.read_csv(meta_path)
    original_stations = meta.shape[0]
    original_values = int(df.notna().sum().sum())

    stations_to_drop = [c for c in BAD_STATIONS if c in df.columns]
    df.drop(columns=stations_to_drop, inplace=True)
    meta = meta[meta.iloc[:, 0].isin(df.columns)].copy()

    n_removed = original_stations - meta.shape[0]
    v_removed = original_values - int(df.notna().sum().sum())
    logger.info(
        "Removed %d stations and %d values (step 1)",
        n_removed, v_removed,
    )

    if qc_cfg.get("outlier_negative", True):
        df[df < 0] = pd.NA
    if qc_cfg.get("outlier_ge", 9999):
        df[df >= qc_cfg["outlier_ge"]] = pd.NA
    if qc_cfg.get("outlier_zero", True):
        df[df == 0] = pd.NA

    v_removed = original_values - int(df.notna().sum().sum())
    logger.info("Removed %d values as outliers", v_removed)

    df = mask_isolated(
        df,
        window=qc_cfg.get("rolling_365_window", 365),
        max_na=qc_cfg.get("rolling_365_max_na", 0.8),
    )
    df = mask_constant(
        df,
        window=qc_cfg.get("rolling_30_window", 30),
        max_cv=qc_cfg.get("rolling_30_max_cv", 0.01),
    )

    for station, ranges in MANUAL_DATE_MASKS.items():
        if station not in df.columns:
            continue
        mask = pd.Series(False, index=df.index)
        for start, end in ranges:
            if start and end:
                mask |= (df.index > start) & (df.index < end)
            elif start:
                mask |= (df.index > start)
            elif end:
                mask |= (df.index < end)
        df.loc[mask, station] = pd.NA

    for station, (op, val) in MANUAL_VALUE_MASKS.items():
        if station not in df.columns:
            continue
        if op == "lt":
            df.loc[df[station] < val, station] = pd.NA
        elif op == "gt":
            df.loc[df[station] > val, station] = pd.NA

    meta["length"] = df.notna().sum().values
    meta.to_csv(Path(output_dir) / "Q_PMETobs_v10_metadata.csv", index=False)

    df.to_csv(Path(output_dir) / "Q_PMETobs_v10d.csv", na_rep="")

    df_m = pd.DataFrame({
        col: monthly_resample(df[col], days_min=qc_cfg.get("n_days", 20))
        for col in df.columns
    })
    df_m.index = df_m.index.date
    df_m.to_csv(Path(output_dir) / "Q_PMETobs_v10m.csv", na_rep="", index_label="Date")

    df_a = pd.DataFrame({
        col: annual_resample(df_m[col], months_min=qc_cfg.get("n_years", 10))
        for col in df_m.columns
    })
    df_a.index = df_a.index.date
    df_a.to_csv(Path(output_dir) / "Q_PMETobs_v10a.csv", na_rep="", index_label="Date")

    logger.info(
        "QC complete: %d stations, %d daily values written",
        meta.shape[0], int(df.notna().sum().sum()),
    )
