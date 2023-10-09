import unittest
from scripts import user


class TestGetProfile(unittest.TestCase):

    def test_valid_profile(self):
        # Test with a valid user profile
        profile = user.get_profile('pycan')
        self.assertIsNotNone(profile)
        self.assertEqual(len(profile), 5)
        self.assertEqual(profile[0], 'pycan')

    def test_invalid_profile(self):
        # Test with an invalid user profile (non-existent user)
        profile = user.get_profile('ycananrino')
        self.assertIsNone(profile)

    # def test_missing_values(self):
    #     # Test with a user profile that has missing values
    #     profile = user.get_profile('saloon')
    #     self.assertIsNotNone(profile)
    #     self.assertEqual(len(profile), 5)
    #     self.assertEqual(profile[0], 'saloon')
    #     self.assertIsNone(profile[1])  # Check if nym_tot_stacked is None
    #     self.assertIsNone(profile[2])  # Check if nym_first_item is None
    #     self.assertIsNone(profile[3])  # Check if nym_ch_streak is None
    #     self.assertIsNone(profile[4])  # Check if nym_tot_items is None

    def test_profile_with_values(self):
        # Test with a user profile that has all values available
        profile = user.get_profile('Bitcoin_squared')
        self.assertIsNotNone(profile)
        self.assertEqual(len(profile), 5)
        self.assertEqual(profile[0], 'Bitcoin_squared')
        self.assertIsNotNone(profile[1])  # Check if nym_tot_stacked is not None
        self.assertIsNotNone(profile[2])  # Check if nym_first_item is not None
        self.assertIsNotNone(profile[3])  # Check if nym_ch_streak is not None
        self.assertIsNotNone(profile[4])  # Check if nym_tot_items is not None


if __name__ == '__main__':
    unittest.main()
