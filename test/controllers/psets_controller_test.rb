require 'test_helper'

class PsetsControllerTest < ActionDispatch::IntegrationTest
  setup do
    @pset = psets(:one)
  end

  test "should get index" do
    get psets_url
    assert_response :success
  end

  test "should get new" do
    get new_pset_url
    assert_response :success
  end

  test "should create pset" do
    assert_difference('Pset.count') do
      post psets_url, params: { pset: {  } }
    end

    assert_redirected_to pset_url(Pset.last)
  end

  test "should show pset" do
    get pset_url(@pset)
    assert_response :success
  end

  test "should get edit" do
    get edit_pset_url(@pset)
    assert_response :success
  end

  test "should update pset" do
    patch pset_url(@pset), params: { pset: {  } }
    assert_redirected_to pset_url(@pset)
  end

  test "should destroy pset" do
    assert_difference('Pset.count', -1) do
      delete pset_url(@pset)
    end

    assert_redirected_to psets_url
  end
end
