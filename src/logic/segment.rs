use super::grammar::Segment;

/// A span representing a range of segments (not bytes/chars)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SegmentRange {
    /// Index of the first segment in the range
    pub start: usize,
    /// Index of the last segment in the range (inclusive)
    pub end: usize,
}

impl SegmentRange {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn single(seg: usize) -> Self {
        Self {
            start: seg,
            end: seg,
        }
    }

    /// Convert segment range to byte range using the segment array
    pub fn to_byte_range(&self, segments: &[Segment]) -> Option<(usize, usize)> {
        let start = segments.get(self.start)?.start;
        let end = segments.get(self.end)?.end;
        Some((start, end))
    }

    /// Merge two segment ranges
    pub fn merge(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}
