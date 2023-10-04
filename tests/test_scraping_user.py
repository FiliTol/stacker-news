import unittest
import numpy as np
from bs4 import BeautifulSoup


from user_modules.scraping_user import (
    get_profile,
    get_total_stacked,
    get_stacking_since,
    get_cowboy_streak,
    get_total_items,
)

# Define a fixed value for missing values or request errors
NA = np.nan

# Create a test class
class TestUserProfileFunctions(unittest.TestCase):

    def test_get_profile(self):
        # Test a valid profile
        profile = get_profile("valid_username")
        self.assertIsInstance(profile, BeautifulSoup)

        # Test an invalid profile
        invalid_profile = get_profile("invalid_username")
        self.assertEqual(invalid_profile, NA)

    def test_get_total_stacked(self):
        # Test a valid profile with total stacked
        total_stacked = get_total_stacked("valid_username_with_total_stacked")
        self.assertIsInstance(total_stacked, int)

        # Test a valid profile without total stacked
        total_stacked_missing = get_total_stacked("valid_username_without_total_stacked")
        self.assertEqual(total_stacked_missing, NA)

        # Test an invalid profile
        invalid_profile = get_total_stacked("invalid_username")
        self.assertEqual(invalid_profile, NA)

    def test_get_stacking_since(self):
        # Test a valid profile with stacking since date
        stacking_since = get_stacking_since("valid_username_with_stacking_since")
        self.assertIsInstance(stacking_since, str)

        # Test a valid profile without stacking since date
        stacking_since_missing = get_stacking_since("valid_username_without_stacking_since")
        self.assertEqual(stacking_since_missing, NA)

        # Test an invalid profile
        invalid_profile = get_stacking_since("invalid_username")
        self.assertEqual(invalid_profile, NA)

    def test_get_cowboy_streak(self):
        # Test a valid profile with cowboy streak
        cowboy_streak = get_cowboy_streak("valid_username_with_cowboy_streak")
        self.assertIsInstance(cowboy_streak, int)

        # Test a valid profile without cowboy streak
        cowboy_streak_missing = get_cowboy_streak("valid_username_without_cowboy_streak")
        self.assertEqual(cowboy_streak_missing, NA)

        # Test an invalid profile
        invalid_profile = get_cowboy_streak("invalid_username")
        self.assertEqual(invalid_profile, NA)

    def test_get_total_items(self):
        # Test a valid profile with total items
        total_items = get_total_items("valid_username_with_total_items")
        self.assertIsInstance(total_items, int)

        # Test a valid profile without total items
        total_items_missing = get_total_items("valid_username_without_total_items")
        self.assertEqual(total_items_missing, NA)

        # Test an invalid profile
        invalid_profile = get_total_items("invalid_username")
        self.assertEqual(invalid_profile, NA)


if __name__ == "__main__":
    unittest.main()
