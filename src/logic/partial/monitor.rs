use std::time::Duration;

#[derive(Debug, Clone, Default)]
pub struct CacheTimingSnapshot {
    pub partial_calls: u64,
    pub partial_total: Duration,
    pub partial_last: Option<Duration>,
    pub lookup_total: Duration,
    pub store_total: Duration,
}

#[derive(Debug, Clone, Default)]
pub struct CacheStatsSnapshot {
    pub enabled: bool,

    pub cache_clears: u64,
    pub cache_invalidations: u64,

    pub lookups: u64,
    pub lookup_hits_exact: u64,
    pub lookup_hits_prefix: u64,
    pub lookup_misses: u64,
    pub lookup_scanned_entries: u64,

    pub stores: u64,
    pub store_inserts: u64,
    pub store_updates: u64,
    pub store_entries_after: u64,

    pub depth_limited_parses: u64,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct CacheMonitor {
    enabled: bool,
    stats: CacheStatsSnapshot,
    timing: CacheTimingSnapshot,
}

impl CacheMonitor {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        self.stats.enabled = enabled;
    }

    pub fn enabled(&self) -> bool {
        self.enabled
    }

    pub fn reset(&mut self) {
        let enabled = self.enabled;
        *self = Self::default();
        self.set_enabled(enabled);
    }

    pub fn stats_snapshot(&self) -> CacheStatsSnapshot {
        self.stats.clone()
    }

    pub fn timing_snapshot(&self) -> CacheTimingSnapshot {
        self.timing.clone()
    }

    pub fn record_partial(&mut self, dur: Duration) {
        if !self.enabled {
            return;
        }
        self.timing.partial_calls += 1;
        self.timing.partial_total += dur;
        self.timing.partial_last = Some(dur);
    }

    pub fn record_cache_clear(&mut self) {
        if !self.enabled {
            return;
        }
        self.stats.cache_clears += 1;
    }

    pub fn record_cache_invalidation(&mut self) {
        if !self.enabled {
            return;
        }
        self.stats.cache_invalidations += 1;
    }

    pub fn record_depth_limited_parse(&mut self) {
        if !self.enabled {
            return;
        }
        self.stats.depth_limited_parses += 1;
    }

    pub fn record_lookup(&mut self, scanned_entries: u64, exact: bool, prefix: bool) {
        if !self.enabled {
            return;
        }
        self.stats.lookups += 1;
        self.stats.lookup_scanned_entries += scanned_entries;
        if exact {
            self.stats.lookup_hits_exact += 1;
        } else if prefix {
            self.stats.lookup_hits_prefix += 1;
        } else {
            self.stats.lookup_misses += 1;
        }
    }

    pub fn record_lookup_time(&mut self, dur: Duration) {
        if !self.enabled {
            return;
        }
        self.timing.lookup_total += dur;
    }

    pub fn record_store_time(&mut self, dur: Duration) {
        if !self.enabled {
            return;
        }
        self.timing.store_total += dur;
    }

    pub fn record_store(&mut self, updates: u64, inserts: u64, total_entries_after: u64) {
        if !self.enabled {
            return;
        }
        self.stats.stores += updates + inserts;
        self.stats.store_updates += updates;
        self.stats.store_inserts += inserts;
        self.stats.store_entries_after = total_entries_after;
    }
}
