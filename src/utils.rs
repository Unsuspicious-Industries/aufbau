// macro and utils

// HashSet macro to create sets easily (legacy - for tests and HashSet usage)
#[macro_export]
macro_rules! set {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_set = std::collections::HashSet::new();
            $(
                temp_set.insert($x);
            )*
            temp_set
        }
    };
}
