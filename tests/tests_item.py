import unittest
from bs4 import BeautifulSoup
from scripts.item import detect_item_type


# Create a class for your tests that inherits from unittest.TestCase
class TestDetectItemType(unittest.TestCase):

    # Helper function to create a mock page with specific elements
    def create_mock_page(self, title_exists=False, is_link=False, is_bounty=False, is_poll=False, is_job=False):
        mock_page = BeautifulSoup("<html></html>", 'html.parser')

        if title_exists:
            title_tag = mock_page.new_tag("a")
            title_tag['class'] = "item_title__FH7AS text-reset me-2"
            title_tag['href'] = "/items/example_item"
            title_tag.string = "Sample Title"
            mock_page.append(title_tag)

        if is_link:
            link_tag = mock_page.new_tag("a")
            link_tag['class'] = "item_link__4cWVs"
            link_tag['target'] = "_blank"
            link_tag.string = "Link Text"
            mock_page.append(link_tag)

        if is_bounty:
            bounty_tag = mock_page.new_tag("div")
            bounty_tag['class'] = "px-3 py-1 d-inline-block bg-grey-medium rounded text-success"
            bounty_tag.string = "Bounty Content"
            mock_page.append(bounty_tag)

        if is_poll:
            poll_tag = mock_page.new_tag("div")
            poll_tag['class'] = "poll_pollBox__Z9Blt"
            poll_tag.string = "Poll Content"
            mock_page.append(poll_tag)

        if is_job:
            job_tag = mock_page.new_tag("a")
            job_tag['class'] = "btn btn-primary"
            job_tag['target'] = "_blank"
            job_tag['tabindex'] = "0"
            job_tag.string = "apply"
            mock_page.append(job_tag)

        return mock_page

    # Test for a link item with title
    def test_detect_link_item(self):
        page = self.create_mock_page(title_exists=True, is_link=True)
        result = detect_item_type(1, page)
        self.assertEqual(result, "link")

    # Test for a bounty item with title
    def test_detect_bounty_item(self):
        page = self.create_mock_page(title_exists=True, is_bounty=True)
        result = detect_item_type(1, page)
        self.assertEqual(result, "bounty")

    # Test for a poll item with title
    def test_detect_poll_item(self):
        page = self.create_mock_page(title_exists=True, is_poll=True)
        result = detect_item_type(1, page)
        self.assertEqual(result, "poll")

    # Test for a job item with title
    def test_detect_job_item(self):
        page = self.create_mock_page(title_exists=True, is_job=True)
        result = detect_item_type(1, page)
        self.assertEqual(result, "job")

    # Test for a discussion item with title
    def test_detect_discussion_item(self):
        page = self.create_mock_page(title_exists=True)
        result = detect_item_type(1, page)
        self.assertEqual(result, "discussion")

    # Test for a comment item (no title)
    def test_detect_comment_item(self):
        page = self.create_mock_page()
        result = detect_item_type(1, page)
        self.assertEqual(result, "comment")


if __name__ == '__main__':
    unittest.main()
