import unittest
from scripts import user
import os
import csv


# Test the get_profile function from user.py
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


# Test the save_profile_csv function from user.py
class TestSaveProfileCSV(unittest.TestCase):

    def setUp(self):
        # Create a temporary directory for testing
        self.test_dir = "data"
        os.makedirs(self.test_dir, exist_ok=True)

    def tearDown(self):
        # Remove the temporary directory and its contents after testing
        os.rmdir(self.test_dir)

    def test_save_profile_csv(self):
        # Define a list of test usernames
        test_user_list = ["k00b", "DarthCoin", "saloon"]

        # Call the function to save profiles to a CSV file
        user.save_profile_csv(test_user_list)

        # Verify that the CSV file has been created
        csv_file_path = os.path.join(self.test_dir, "profiles.csv")
        self.assertTrue(os.path.isfile(csv_file_path))

        # Check if the CSV file contains the expected rows
        with open(csv_file_path, 'r', encoding='utf_8_sig') as csvfile:
            csvreader = csv.reader(csvfile)
            rows = list(csvreader)

        # Check the header row
        self.assertEqual(rows[0], ["User", "Total stacked", "First item", "Max Cowboy-hat streak", "Total user items"])

        # Check the data rows
        expected_data = [
            ["k00b", 4685779, "1", 134, 10032],
            ["DarthCoin", 1309597, "166", 77, 10171],
            ["saloon", 0, "7406", None, 642]
        ]

        for i in range(len(expected_data)):
            self.assertEqual(rows[i + 1], [str(value) for value in expected_data[i]])


if __name__ == '__main__':
    unittest.main()
