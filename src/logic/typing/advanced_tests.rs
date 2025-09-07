//! Advanced Type System Tests for new functionality
//! 
//! Tests for Union, Intersection, Pointer, and Array types

#[cfg(test)]
pub mod advanced_type_system_tests {
    use crate::logic::typing::{Type, ArraySize};

    #[test] 
    fn test_type_parsing_advanced() {
        // Test Union type parsing
        let union_type = Type::parse("Int ∨ Bool").unwrap();
        assert!(matches!(union_type, Type::Union(_, _)));
        
        // Test Intersection type parsing
        let intersection_type = Type::parse("Int ∧ String").unwrap();
        assert!(matches!(intersection_type, Type::Intersection(_, _)));
        
        // Test Pointer type parsing
        let pointer_type = Type::parse("*Int").unwrap();
        assert!(matches!(pointer_type, Type::Pointer(_)));
        
        // Test Array type parsing
        let array_type = Type::parse("Int[10]").unwrap();
        assert!(matches!(array_type, Type::Array(_, ArraySize::Const(10))));
        
        let dynamic_array_type = Type::parse("Int[]").unwrap();
        assert!(matches!(dynamic_array_type, Type::Array(_, ArraySize::Dynamic)));

        let symbolic_array_type = Type::parse("Int[N]").unwrap();
        assert!(matches!(symbolic_array_type, Type::Array(_, ArraySize::Var(_))));
    }
    
    #[test]
    fn test_union_type_compatibility() {
        let int_type = Type::parse("Int").unwrap();
        let bool_type = Type::parse("Bool").unwrap();
        let union_int_bool = Type::parse("Int ∨ Bool").unwrap();
        
        // Int should be compatible with Int ∨ Bool (subtype)
        assert!(int_type.is_compatible_with(&union_int_bool), "Int should be compatible with Int ∨ Bool");
        
        // Bool should be compatible with Int ∨ Bool (subtype)
        assert!(bool_type.is_compatible_with(&union_int_bool), "Bool should be compatible with Int ∨ Bool");
        
        // Int ∨ Bool should NOT be compatible with Int (not a subtype)
        assert!(!union_int_bool.is_compatible_with(&int_type), "Int ∨ Bool should not be compatible with Int");
    }
    
    #[test]
    fn test_intersection_type_compatibility() {
        let int_type = Type::parse("Int").unwrap();
        let bool_type = Type::parse("Bool").unwrap();
        let intersection_int_bool = Type::parse("Int ∧ Bool").unwrap();
        
        // Int ∧ Bool should be compatible with Int (subtype)
        assert!(intersection_int_bool.is_compatible_with(&int_type), "Int ∧ Bool should be compatible with Int");
        
        // Int ∧ Bool should be compatible with Bool (subtype)
        assert!(intersection_int_bool.is_compatible_with(&bool_type), "Int ∧ Bool should be compatible with Bool");
        
        // Int should NOT be compatible with Int ∧ Bool (not a subtype)
        assert!(!int_type.is_compatible_with(&intersection_int_bool), "Int should not be compatible with Int ∧ Bool");
    }
    
    #[test]
    fn test_pointer_type_compatibility() {
        let int_type = Type::parse("Int").unwrap();
        let int_ptr = Type::parse("*Int").unwrap();
        let bool_ptr = Type::parse("*Bool").unwrap();
        
        // *Int should be compatible with *Int
        assert!(int_ptr.is_compatible_with(&int_ptr), "*Int should be compatible with *Int");
        
        // *Int should NOT be compatible with *Bool (different pointer types)
        assert!(!int_ptr.is_compatible_with(&bool_ptr), "*Int should not be compatible with *Bool");
        
        // *Int should NOT be compatible with Int (different types)
        assert!(!int_ptr.is_compatible_with(&int_type), "*Int should not be compatible with Int");
        assert!(!int_type.is_compatible_with(&int_ptr), "Int should not be compatible with *Int");
    }
    
    #[test]
    fn test_array_type_compatibility() {
        let int_array_10 = Type::parse("Int[10]").unwrap();
        let int_array_20 = Type::parse("Int[20]").unwrap();
        let int_array_dyn = Type::parse("Int[]").unwrap();
        let bool_array_10 = Type::parse("Bool[10]").unwrap();
        
        // Int[10] should be compatible with Int[10]
        assert!(int_array_10.is_compatible_with(&int_array_10), "Int[10] should be compatible with Int[10]");
        
        // Int[10] should NOT be compatible with Int[20] (different sizes)
        assert!(!int_array_10.is_compatible_with(&int_array_20), "Int[10] should not be compatible with Int[20]");
        
        // Int[10] should NOT be compatible with Bool[10] (different element types)
        assert!(!int_array_10.is_compatible_with(&bool_array_10), "Int[10] should not be compatible with Bool[10]");
        
        // Int[10] should NOT be compatible with Int[] (fixed vs dynamic)
        assert!(!int_array_10.is_compatible_with(&int_array_dyn), "Int[10] should not be compatible with Int[]");
    }
    
    #[test]
    fn test_arrow_type_compatibility() {
        let int_to_bool = Type::parse("Int -> Bool").unwrap();
        let bool_to_int = Type::parse("Bool -> Int").unwrap();
        let int_to_int = Type::parse("Int -> Int").unwrap();
        
        // Same arrow types should be compatible
        assert!(int_to_bool.is_compatible_with(&int_to_bool), "Int -> Bool should be compatible with Int -> Bool");
        
        // Different arrow types should not be compatible
        assert!(!int_to_bool.is_compatible_with(&bool_to_int), "Int -> Bool should not be compatible with Bool -> Int");
        assert!(!int_to_bool.is_compatible_with(&int_to_int), "Int -> Bool should not be compatible with Int -> Int");
    }
    
    #[test]
    fn test_complex_type_expressions() {
        // Test complex nested types - first build it step by step
        let union_type = Type::parse("Int ∨ Bool").unwrap();
        let pointer_union = Type::Pointer(Box::new(union_type));
        let array_of_pointer_union = Type::Array(Box::new(pointer_union), ArraySize::Const(10));
        
        // Verify the structure
        assert!(matches!(array_of_pointer_union, Type::Array(_, ArraySize::Const(10))));
        
        if let Type::Array(elem_type, _) = array_of_pointer_union {
            assert!(matches!(elem_type.as_ref(), Type::Pointer(_)));
            if let Type::Pointer(pointed_type) = elem_type.as_ref() {
                assert!(matches!(pointed_type.as_ref(), Type::Union(_, _)));
            }
        }
    }
    
    #[test]
    fn test_display_format() {
        let union_type = Type::parse("Int ∨ Bool").unwrap();
        assert_eq!(format!("{}", union_type), "Int ∨ Bool");
        
        let intersection_type = Type::parse("Int ∧ Bool").unwrap();
        assert_eq!(format!("{}", intersection_type), "Int ∧ Bool");
        
        let pointer_type = Type::parse("*Int").unwrap();
        assert_eq!(format!("{}", pointer_type), "*Int");
        
        let array_type = Type::parse("Int[10]").unwrap();
        assert_eq!(format!("{}", array_type), "Int[10]");
        
        let dynamic_array_type = Type::parse("Int[]").unwrap();
        assert_eq!(format!("{}", dynamic_array_type), "Int[]");

        let symbolic_array_type = Type::parse("Int[N]").unwrap();
        assert_eq!(format!("{}", symbolic_array_type), "Int[N]");
    }
}